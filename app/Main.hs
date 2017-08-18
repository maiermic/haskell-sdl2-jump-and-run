{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tile (Tile(..))
import TileMap (TileMap(..), readTileMap)

import Control.Concurrent (threadDelay)
import Control.Monad (guard, unless, when)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Debug.Trace (trace)
import Foreign.C.Types (CInt)
import GHC.Word (Word32, Word8(..))
import SDL (($=))
import qualified SDL
import SDL.Font (Font(..))
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Raw
import SDL.Vect (Point(..), V2(..), V4(..))
import SDL.Video.Renderer (Rectangle(..))
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import Paths_sdl2_jump_and_run (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1280, 720)

type Tick = Word32

type Color = V4 Word8

type Vector2d = V2 Float

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = V4 r g b maxBound

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Color
rgba = V4

rect x y w h = SDL.Rectangle (P $ V2 x y) (V2 w h)

data Input
  = None
  | MoveLeft
  | MoveRight
  | Jump
  | Restart
  | Quit
  deriving (Eq)

data Time = Time
  { begin, finish, best :: !Int
  } deriving (Show)

newTime = Time {begin = 0, finish = -1, best = -1}

data Player = Player
  { texture :: SDL.Texture
  , position :: !(Point V2 Float)
  , velocity :: !Vector2d
  , time :: !Time
  }

instance Show Player where
  show Player {position = P (V2 posX posY), velocity = V2 velX velY} =
    "Player(position=" ++
    show (posX, posY) ++ ",velocity=" ++ show (velX, velY) ++ ")"

createPlayer :: SDL.Texture -> Player
createPlayer t =
  Player
  {texture = t, position = P $ V2 170 500, velocity = V2 0 0, time = newTime}

restartPlayer :: Player -> Player
restartPlayer p@Player {texture, time = Time {best}} =
  (createPlayer texture) {time = newTime {best}}

data Game = Game
  { player :: !Player
  , tileMap :: !TileMap
  , camera :: !Vector2d
  , font :: Font
  }

ticksPerSecond = 60

tilesPerRow = 16 :: Int

tileWidth = 64 :: CInt

tileHeight = 64 :: CInt

playerWidth = 64 :: Float

playerHeight = 64 :: Float

playerSize = V2 playerWidth playerHeight

toInput :: SDL.Keycode -> Input
toInput =
  \case
    SDL.KeycodeA -> MoveLeft
    SDL.KeycodeD -> MoveRight
    SDL.KeycodeSpace -> Jump
    SDL.KeycodeR -> Restart
    SDL.KeycodeQ -> Quit
    _ -> None

isPressedKey :: SDL.KeyboardEventData -> Bool
isPressedKey eventData = SDL.Pressed == SDL.keyboardEventKeyMotion eventData

toPressedKey :: SDL.EventPayload -> Maybe SDL.Keycode
toPressedKey (SDL.KeyboardEvent e)
  | isPressedKey e = Just $ SDL.keysymKeycode $ SDL.keyboardEventKeysym e
toPressedKey _ = Nothing

isReleasedKey :: SDL.KeyboardEventData -> Bool
isReleasedKey eventData = SDL.Released == SDL.keyboardEventKeyMotion eventData

toReleasedKey :: SDL.EventPayload -> Maybe SDL.Keycode
toReleasedKey (SDL.KeyboardEvent e)
  | isReleasedKey e = Just $ SDL.keysymKeycode $ SDL.keyboardEventKeysym e
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
  let (P (V2 x y)) = position
      backFeetShadow =
        CopyExData
        { source = SDL.Rectangle (P $ V2 192 64) (V2 64 32)
        , destination = SDL.Rectangle (P $ V2 (x - 60) y) (V2 96 48)
        , flips = flipNone
        }
      bodyShadow =
        CopyExData
        { source = SDL.Rectangle (P $ V2 96 0) (V2 96 96)
        , destination = SDL.Rectangle (P $ V2 (x - 48) (y - 48)) (V2 96 96)
        , flips = flipNone
        }
      frontFeetShadow =
        CopyExData
        { source = SDL.Rectangle (P $ V2 192 64) (V2 64 32)
        , destination = SDL.Rectangle (P $ V2 (x - 36) y) (V2 96 48)
        , flips = flipNone
        }
      backFeet =
        CopyExData
        { source = SDL.Rectangle (P $ V2 192 32) (V2 64 32)
        , destination = SDL.Rectangle (P $ V2 (x - 60) y) (V2 96 48)
        , flips = flipNone
        }
      body =
        CopyExData
        { source = SDL.Rectangle (P $ V2 0 0) (V2 96 96)
        , destination = SDL.Rectangle (P $ V2 (x - 48) (y - 48)) (V2 96 96)
        , flips = flipNone
        }
      frontFeet =
        CopyExData
        { source = SDL.Rectangle (P $ V2 192 32) (V2 64 32)
        , destination = SDL.Rectangle (P $ V2 (x - 36) y) (V2 96 48)
        , flips = flipNone
        }
      leftEye =
        CopyExData
        { source = SDL.Rectangle (P $ V2 64 96) (V2 32 32)
        , destination = SDL.Rectangle (P $ V2 (x - 18) (y - 21)) (V2 36 36)
        , flips = flipNone
        }
      rightEye =
        CopyExData
        { source = SDL.Rectangle (P $ V2 64 96) (V2 32 32)
        , destination = SDL.Rectangle (P $ V2 (x - 6) (y - 21)) (V2 36 36)
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
      copy bodyPart@CopyExData {source, destination, flips} =
        SDL.copyEx
          renderer
          texture
          (Just source)
          (Just destination)
          angle
          rotation
          flips
  in mapM copy bodyParts

xCoordinates :: Int -> Int -> [Int]
xCoordinates columns rows = take (rows * columns) $ cycle [0 .. (columns - 1)]

yCoordinates :: Int -> Int -> [Int]
yCoordinates columns rows = concatMap (replicate columns) [0 .. (rows - 1)]

renderTileMap :: SDL.Renderer -> TileMap -> Vector2d -> IO [()]
renderTileMap renderer TileMap {texture, tiles, width, height} (V2 cameraX cameraY) =
  let xCoordinates' = map fromIntegral $ xCoordinates width height
      yCoordinates' = map fromIntegral $ yCoordinates width height
      tile x y = SDL.Rectangle (P $ V2 x y) (V2 tileWidth tileHeight)
      clipX tileNr = fromIntegral (tileNr `mod` tilesPerRow) * tileWidth
      clipY tileNr = fromIntegral (tileNr `div` tilesPerRow) * tileHeight
      clip tileNr = tile (clipX tileNr) (clipY tileNr)
      destX x = x * tileWidth - round cameraX
      destY y = y * tileHeight - round cameraY
      dest x y = tile (destX x) (destY y)
      isEmptyTile (_, _, tile) = tile == Tile.Air
      copy coord =
        let (x, y, tile) = coord
        in SDL.copy
             renderer
             texture
             (Just $ clip $ fromEnum tile)
             (Just $ dest x y)
  in mapM copy $
     filter (not . isEmptyTile) $ zip3 xCoordinates' yCoordinates' tiles

getTile :: TileMap -> Float -> Float -> Tile
getTile TileMap {width, height, tiles} x y =
  let nx = clamp 0 (width - 1) (round x `div` fromIntegral tileWidth)
      ny = clamp 0 (height - 1) (round y `div` fromIntegral tileHeight)
      pos = ny * width + nx
  in tiles !! pos

isSolid :: TileMap -> Float -> Float -> Bool
isSolid tileMap x y =
  getTile tileMap x y `notElem` [Tile.Air, Tile.Start, Tile.Finish]

onGround :: TileMap -> Point V2 Float -> Vector2d -> Bool
onGround tileMap position size =
  let (P (V2 posX posY)) = position
      V2 width height = size * 0.5
  in or $
     uncurry (isSolid tileMap) <$>
     [(posX - width, posY + height + 1), (posX + width, posY + height + 1)]

testBox :: TileMap -> Point V2 Float -> Vector2d -> Bool
testBox tileMap position size =
  let (P (V2 posX posY)) = position
      V2 width height = size * 0.5
  in or $
     isSolid tileMap <$> [posX - width, posX + width] <*>
     [posY - height, posY + height]

len :: Vector2d -> Float
len v = sqrt $ sum $ v ** 2

toFloat :: Integral a => a -> Float
toFloat = fromIntegral

moveBox ::
     TileMap
  -> Point V2 Float
  -> Vector2d
  -> Vector2d
  -> (Point V2 Float, Vector2d)
moveBox tileMap position velocity size =
  let distance = len velocity
      maximum = round distance
      fraction = 1.0 / fromIntegral (maximum + 1)
      moveBox' size (pos@(P (V2 posX posY)), vel@(V2 velX velY)) i =
        let newPos@(P (V2 newPosX newPosY)) = pos + P vel * fraction
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
        in if isHit
             then (P (V2 posX' posY'), V2 velX' velY')
             else (newPos, vel)
  in if distance < 0
       then (position, velocity)
       else foldl (moveBox' size) (position, velocity) [0 .. maximum]

renderText :: SDL.Renderer -> Font -> Color -> String -> CInt -> CInt -> IO ()
renderText renderer font color text x y =
  let outlineColor = rgba 0 0 0 64
      renderText' :: CInt -> Color -> IO ()
      renderText' outline color = do
        SDL.Font.setOutline font $ fromIntegral outline
        surface <- SDL.Font.blended font color $ pack text
        texture <- SDL.createTextureFromSurface renderer surface
        V2 surfaceWidth surfaceHeight <- SDL.surfaceDimensions surface
        SDL.freeSurface surface
        let source = rect 0 0 surfaceWidth surfaceHeight
            dest = rect (x - outline) (y - outline) surfaceWidth surfaceHeight
            angle = 0.0
            center = Nothing
        SDL.copyEx
          renderer
          texture
          (Just source)
          (Just dest)
          angle
          center
          flipNone
        SDL.destroyTexture texture
  in do renderText' 2 outlineColor
        renderText' 0 color

renderPlayerTime :: SDL.Renderer -> Game -> Tick -> IO ()
renderPlayerTime renderer game tick =
  let Time {begin, finish, best} = time $ player game
      white = rgb maxBound maxBound maxBound
      renderText' = renderText renderer (font game) white
  in do if begin >= 0
          then renderText' (formatTime $ tick - fromIntegral begin) 50 100
          else when (finish >= 0) $
               renderText'
                 ("Finished in: " ++ formatTime (fromIntegral finish))
                 50
                 100
        when (best >= 0) $
          renderText' ("Best time: " ++ formatTime (fromIntegral best)) 50 150

renderGame :: SDL.Renderer -> Game -> Tick -> IO ()
renderGame renderer game@Game {player, tileMap, camera} tick = do
  SDL.clear renderer
  renderTee
    renderer
    (texture (player :: Player))
    (round <$> position player - P camera)
  renderTileMap renderer tileMap camera
  renderPlayerTime renderer game tick
  SDL.present renderer

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

physics :: [Input] -> Game -> Game
physics inputs game@Game {player, tileMap} =
  let player' =
        if Restart `elem` inputs
          then restartPlayer player
          else updatePlayer tileMap inputs player
  in game {player = player'}

updatePlayer tileMap inputs player =
  let is input = input `elem` inputs
      Player {velocity = (V2 vx vy), position} = player
      isOnGround = onGround tileMap position playerSize
      jump =
        if is Jump && isOnGround
          then -21
          else 0
      gravity = 0.75
      vy' = sum [vy, jump, gravity]
      r =
        if is MoveRight
          then 1
          else 0
      l =
        if is MoveLeft
          then 1
          else 0
      direction = (r - l) :: Float
      vx' =
        clamp (-8) 8 $
        if isOnGround
          then 0.5 * vx + 4.0 * direction
          else 0.95 * vx + 2.0 * direction
      (position', velocity') = moveBox tileMap position (V2 vx' vy') playerSize
  in player {position = position', velocity = velocity'}

without values excludes = filter (`notElem` excludes) values

moveCamera game@Game { player = Player {position = P (V2 px _)}
                     , camera = V2 cx cy
                     } =
  game {camera = V2 (px - fromIntegral screenWidth / 2) cy}

formatTime :: Tick -> String
formatTime tick =
  let mins = (tick `div` ticksPerSecond) `div` 60
      secs = (tick `div` ticksPerSecond) `mod` 60
      cents = (tick `mod` ticksPerSecond) * 2
  in printf "%02d:%02d:%02d" mins secs cents

updateTime :: Tile -> Tick -> Time -> Time
updateTime tile tick time@Time {begin, finish, best} =
  case tile of
    Tile.Start -> time {begin = fromIntegral tick}
    Tile.Finish
      | begin >= 0 ->
        let finish' = fromIntegral tick - begin
        in time
           { finish = finish'
           , begin = -1
           , best =
               if best < 0 || finish' < best
                 then finish'
                 else best
           }
    _ -> time

logic :: Tick -> Game -> Game
logic tick game@Game {tileMap, player} =
  let Player {time, position = P (V2 px py)} = player
  in game
     {player = player {time = updateTime (getTile tileMap px py) tick time}}

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
  defaultTileMap <-
    getDataFileName "default.map" >>= readFile >>= return <$> readTileMap
  SDL.Font.initialize
  font <- getDataFileName "DejaVuSans.ttf" >>= flip SDL.Font.load 28
  let game =
        Game
        { player = createPlayer playerTexture
        , tileMap = defaultTileMap {texture = tileMapTexture}
        , camera = V2 0 0
        , font
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
        let tickTime = 1000 `div` ticksPerSecond
        let tick = currentTime `div` tickTime
        if dt > tickTime
          then do
            let newGame = logic tick $ moveCamera $ physics inputs game
            renderGame renderer newGame tick
            unless quit $ loop newGame keysDown' (lastTime + tickTime)
          else do
            SDL.delay 1
            unless quit $ loop game keysDown' lastTime
  startTime <- SDL.Raw.getTicks
  loop game [] startTime
  SDL.Font.quit
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit