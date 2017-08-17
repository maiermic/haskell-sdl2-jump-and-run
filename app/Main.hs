{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import TileMap (TileMap(..), readTileMap)

import Data.List (intercalate)
import System.CPUTime (getCPUTime)
import Debug.Trace (trace)
import Control.Monad (guard, when, unless)
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)
import Foreign.C.Types (CInt)
import SDL.Vect (Point(..), V2(..), V4(..))
import SDL.Video.Renderer (Rectangle(..))
import SDL (($=))
import GHC.Word (Word8(..))
import qualified SDL
import qualified SDL.Image
import qualified SDL.Raw

import Paths_sdl2_jump_and_run (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1280, 720)

type Color = V4 Word8
type Vector2d = V2 Float

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = V4 r g b maxBound

data Input
  = None
  | MoveLeft
  | MoveRight
  | Jump
  | Restart
  | Quit
  deriving (Eq)

data Player = Player
  { texture :: SDL.Texture
  , position :: !(Point V2 Float)
  , velocity :: !Vector2d
  }

instance Show Player where
  show Player{position = P (V2 posX posY), velocity = V2 velX velY} =
    "Player(position=" ++ show (posX, posY) ++ ",velocity=" ++ show (velX, velY) ++ ")"

createPlayer :: SDL.Texture -> Player
createPlayer t = Player
  { texture = t
  , position = P $ V2 170 500
  , velocity = V2 0 0
  }

restartPlayer :: Player -> Player
restartPlayer p = createPlayer $ texture (p :: Player)

data Game = Game
  { player :: !Player
  , tileMap :: !TileMap
  , camera :: !Vector2d
  }


tilesPerRow = 16 :: Int
tileWidth = 64 :: CInt
tileHeight = 64 :: CInt

playerWidth = 64 :: Float
playerHeight = 64 :: Float
playerSize = V2 playerWidth playerHeight

-- tile numbers
air = 0
start = 78
finish = 110


toInput :: SDL.Keycode -> Input
toInput = \case
  SDL.KeycodeA -> MoveLeft
  SDL.KeycodeD -> MoveRight
  SDL.KeycodeSpace -> Jump
  SDL.KeycodeR -> Restart
  SDL.KeycodeQ -> Quit
  _ -> None

isPressedKey :: SDL.KeyboardEventData -> Bool
isPressedKey eventData = SDL.Pressed == SDL.keyboardEventKeyMotion eventData

toPressedKey :: SDL.EventPayload -> Maybe SDL.Keycode
toPressedKey (SDL.KeyboardEvent e) | isPressedKey e = Just $ SDL.keysymKeycode $ SDL.keyboardEventKeysym e
toPressedKey _ = Nothing

isReleasedKey :: SDL.KeyboardEventData -> Bool
isReleasedKey eventData = SDL.Released == SDL.keyboardEventKeyMotion eventData

toReleasedKey :: SDL.EventPayload -> Maybe SDL.Keycode
toReleasedKey (SDL.KeyboardEvent e) | isReleasedKey e = Just $ SDL.keysymKeycode $ SDL.keyboardEventKeysym e
toReleasedKey _ = Nothing

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture renderer filePath =
  getDataFileName filePath >>= SDL.Image.loadTexture renderer

data CopyExData = CopyExData
  { source :: SDL.Rectangle CInt
  , destination :: SDL.Rectangle CInt
  , flips :: V2 Bool
  }

flipNone = V2 False False
flipHorizontal = V2 False True
flipVertical = V2 True False

renderTee :: SDL.Renderer -> SDL.Texture -> Point V2 CInt -> IO [()]
renderTee renderer texture position =
  let
    (P (V2 x y)) = position
    backFeetShadow = CopyExData
      { source = SDL.Rectangle (P $ V2 192 64) (V2 64 32)
      , destination = SDL.Rectangle (P $ V2 (x - 60) y) (V2 96 48)
      , flips = flipNone
      }
    bodyShadow = CopyExData
      { source = SDL.Rectangle (P $ V2 96 0) (V2 96 96)
      , destination = SDL.Rectangle (P $ V2 (x - 48) (y - 48)) (V2 96 96)
      , flips = flipNone
      }
    frontFeetShadow = CopyExData
      { source = SDL.Rectangle (P $ V2 192 64) (V2 64 32)
      , destination = SDL.Rectangle (P $ V2 (x - 36) y) (V2 96 48)
      , flips = flipNone
      }
    backFeet = CopyExData
      { source = SDL.Rectangle (P $ V2 192 32) (V2 64 32)
      , destination = SDL.Rectangle (P $ V2 (x - 60) y) (V2 96 48)
      , flips = flipNone
      }
    body = CopyExData
      { source = SDL.Rectangle (P $ V2  0 0) (V2 96 96)
      , destination = SDL.Rectangle (P $ V2 (x - 48) (y - 48)) (V2 96 96)
      , flips = flipNone
      }
    frontFeet = CopyExData
      { source = SDL.Rectangle (P $ V2 192 32) (V2 64 32)
      , destination = SDL.Rectangle (P $ V2 (x - 36) y) (V2 96 48)
      , flips = flipNone
      }
    leftEye = CopyExData
      { source = SDL.Rectangle (P $ V2 64 96) (V2 32 32)
      , destination = SDL.Rectangle (P $ V2 (x - 18) (y - 21)) (V2 36 36)
      , flips = flipNone
      }
    rightEye = CopyExData
      { source = SDL.Rectangle (P $ V2 64 96) (V2 32 32)
      , destination = SDL.Rectangle (P $ V2  (x - 6) (y - 21)) (V2 36 36)
      , flips = flipHorizontal
      }
    bodyParts =
      [ backFeetShadow
      , bodyShadow
      , frontFeetShadow
      , backFeet
      , body
      , frontFeet
      , leftEye
      , rightEye
      ]
    angle = 0.0
    rotation = Nothing
    copy bodyPart@(CopyExData {source, destination, flips}) =
      SDL.copyEx renderer texture (Just source) (Just destination) angle rotation flips
  in
    mapM copy bodyParts

xCoordinates :: Int -> Int -> [Int]
xCoordinates columns rows = take (rows * columns) $ cycle [0..(columns - 1)]

yCoordinates :: Int -> Int -> [Int]
yCoordinates columns rows = concat $ map (take columns . repeat) [0..(rows - 1)]

renderTileMap :: SDL.Renderer -> TileMap -> Vector2d -> IO [()]
renderTileMap renderer (TileMap {texture, tiles, width, height}) (V2 cameraX cameraY) =
  let
    xCoordinates' = map fromIntegral $ xCoordinates width height
    yCoordinates' = map fromIntegral $ yCoordinates width height
    tile x y = SDL.Rectangle (P $ V2 x y) (V2 tileWidth tileHeight)
    clipX tileNr = fromIntegral(tileNr `mod` tilesPerRow) * tileWidth
    clipY tileNr = fromIntegral(tileNr `div` tilesPerRow) * tileHeight
    clip tileNr = tile (clipX tileNr) (clipY tileNr)
    destX x = x * tileWidth - (round cameraX)
    destY y = y * tileHeight - (round cameraY)
    dest x y = tile (destX x) (destY y)
    isEmptyTile (x, y, tileNr) = tileNr == 0
    copy coord =
      let (x, y, tileNr) = coord
      in SDL.copy renderer texture (Just $ clip tileNr) (Just $ dest x y)
  in
    mapM copy $ filter (not . isEmptyTile) $ zip3 xCoordinates' yCoordinates' tiles

getTile :: TileMap -> Float -> Float -> Int
getTile TileMap{width, height, tiles} x y =
  let
    nx = clamp 0 (width - 1) (round x `div` fromIntegral tileWidth)
    ny = clamp 0 (height - 1) (round y `div` fromIntegral tileHeight)
    pos = ny * width + nx
  in
    tiles !! pos

isSolid :: TileMap -> Float -> Float -> Bool
isSolid tileMap x y =
  getTile tileMap x y `notElem` [air, start, finish]

onGround :: TileMap -> Point V2 Float -> Vector2d -> Bool
onGround tileMap position size =
  let
    (P (V2 posX posY)) = position
    V2 width height = size * 0.5
  in
    or $ uncurry (isSolid tileMap) <$>
      [ (posX - width, posY + height + 1)
      , (posX + width, posY + height + 1)
      ]

testBox :: TileMap -> Point V2 Float -> Vector2d -> Bool
testBox tileMap position size =
  let
    (P (V2 posX posY)) = position
    V2 width height = size * 0.5
  in
    or $ isSolid tileMap <$> [posX - width, posX + width] <*> [posY - height, posY + height]

len :: Vector2d -> Float
len v = sqrt $ sum $ v ** 2

toFloat :: Integral a => a -> Float
toFloat = fromIntegral

moveBox :: TileMap -> Point V2 Float -> Vector2d  -> Vector2d -> (Point V2 Float, Vector2d)
moveBox tileMap position velocity size =
  let
    distance = len velocity
    maximum = round distance
    fraction = 1.0 / fromIntegral (maximum + 1)
    moveBox' size (pos@(P (V2 posX posY)), vel@(V2 velX velY)) i =
      let
        newPos@(P (V2 newPosX newPosY)) = pos + P vel * fraction
        isHit = testBox tileMap newPos size
        isHitX = testBox tileMap (P $ V2 newPosX posY) size
        isHitY = testBox tileMap (P $ V2 posX newPosY) size
        isHitCorner = not (isHitX || isHitY)
        (posX', velX') =
          if isHitX || isHitCorner
          then (posX, 0) -- stop x-movement at current x-position
          else (newPosX, velX)
        (posY', velY') =
          if isHitY || isHitCorner
          then (posY, 0) -- stop y-movement at current y-position
          else (newPosY, velY)
      in
        if isHit
        then (P (V2 posX' posY'), V2 velX' velY')
        else (newPos, vel)
  in
    if distance < 0
    then (position, velocity)
    else foldl (moveBox' size) (position, velocity) [0..maximum]

renderGame :: SDL.Renderer -> Game -> IO ()
renderGame renderer Game{player, tileMap, camera} = do
  SDL.clear renderer
  renderTee renderer (texture (player :: Player)) (round <$> position player - P camera)
  renderTileMap renderer tileMap camera
  SDL.present renderer

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

physics :: [Input] -> Game -> Game
physics inputs game@(Game{player, tileMap}) =
  let
    player' =
      if Restart `elem` inputs
      then restartPlayer player
      else updatePlayer tileMap inputs player
  in
    game
      { player = player'
      }

updatePlayer tileMap inputs player =
  let
    is input = input `elem` inputs
    Player {velocity = (V2 vx vy), position} = player
    isOnGround = onGround tileMap position playerSize
    jump = if is Jump && isOnGround then -21 else 0
    gravity = 0.75
    vy' = sum [vy, jump, gravity]
    r = if is MoveRight then 1 else 0
    l = if is MoveLeft then 1 else 0
    direction = (r - l) :: Float
    vx' = clamp (-8) 8 $
      if isOnGround
      then 0.5 * vx + 4.0 * direction
      else 0.95 * vx + 2.0 * direction
    (position', velocity') = moveBox tileMap position (V2 vx' vy') playerSize
  in
    player
     { position = position'
     , velocity = velocity'
     }

without values excludes = filter (`notElem` excludes) values

moveCamera game@Game{player = Player{position = P (V2 px _)}, camera = V2 cx cy } =
  game { camera = V2 (px - fromIntegral screenWidth / 2) cy }


main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitTimer, SDL.InitEvents]
  -- ensure render quality
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
      SDL.createWindow
        "Our own 2D platformer"
        SDL.defaultWindow
          { SDL.windowPosition = SDL.Centered
          , SDL.windowInitialSize = V2 screenWidth screenHeight
          }
  SDL.showWindow window

  renderer <-
      SDL.createRenderer
        window
        (-1)
        SDL.RendererConfig
          { SDL.rendererType = SDL.AcceleratedVSyncRenderer
          , SDL.rendererTargetTexture = True
          }
  SDL.rendererDrawColor renderer $= rgb 110 132 174

  playerTexture <- loadTexture renderer "player.png"
  tileMapTexture <- loadTexture renderer "grass.png"
  defaultTileMap <- getDataFileName "default.map" >>= readFile >>= return <$> readTileMap
  let
    game = Game
      { player = createPlayer playerTexture
      , tileMap = defaultTileMap
          { texture = tileMapTexture
          }
      , camera = V2 0 0
      }
    loop game keysDown lastTime = do
      events <- map SDL.eventPayload <$> SDL.pollEvents
      let pressedKeys = mapMaybe toPressedKey events
      let releasedKeys = mapMaybe toReleasedKey events
      let keysDown' = (keysDown ++ pressedKeys) `without` releasedKeys
      let inputs = map toInput keysDown'
      let quit = (SDL.QuitEvent `elem` events) || (Quit `elem` inputs)
      currentTime <- SDL.Raw.getTicks
      let dt = currentTime - lastTime
      let tickTime = 1000 `div` 60
      if dt > tickTime then do
        let newGame = moveCamera $ physics inputs game
        renderGame renderer newGame
        unless quit $ loop newGame keysDown' (lastTime + tickTime)
      else do
        SDL.delay 1
        unless quit $ loop game keysDown' lastTime

  startTime <- SDL.Raw.getTicks
  loop game [] startTime

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
