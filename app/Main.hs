{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when, unless)
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)
import Foreign.C.Types (CInt)
import SDL.Vect (Point(..), V2(..), V4(..))
import SDL (($=))
import GHC.Word (Word8(..))
import qualified SDL
import qualified SDL.Image
import qualified SDL.Raw

import Paths_sdl2_jump_and_run (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1280, 720)

type Color = V4 Word8
type Vector2d = V2 CInt
type Point2d = Point V2 CInt

p2 :: CInt -> CInt -> Point2d
p2 x y = P $ V2 x y

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
  { texture :: !SDL.Texture
  , position :: !Point2d
  , velocity :: !Vector2d
  }

createPlayer :: SDL.Texture -> Player
createPlayer t = Player
  { texture = t
  , position = p2 170 500
  , velocity = V2 0 0
  }

restartPlayer :: Player -> Player
restartPlayer p = createPlayer $ texture p

data Game = Game
  { player :: !Player
  , camera :: !Vector2d
  }

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

renderTee :: SDL.Renderer -> SDL.Texture -> Point2d -> IO [()]
renderTee renderer texture position =
  let
    (P (V2 x y)) = position
    backFeetShadow = CopyExData
      { source = SDL.Rectangle (p2 192 64) (V2 64 32)
      , destination = SDL.Rectangle (p2 (x - 60) y) (V2 96 48)
      , flips = flipNone
      }
    bodyShadow = CopyExData
      { source = SDL.Rectangle (p2 96 0) (V2 96 96)
      , destination = SDL.Rectangle (p2 (x - 48) (y - 48)) (V2 96 96)
      , flips = flipNone
      }
    frontFeetShadow = CopyExData
      { source = SDL.Rectangle (p2 192 64) (V2 64 32)
      , destination = SDL.Rectangle (p2 (x - 36) y) (V2 96 48)
      , flips = flipNone
      }
    backFeet = CopyExData
      { source = SDL.Rectangle (p2 192 32) (V2 64 32)
      , destination = SDL.Rectangle (p2 (x - 60) y) (V2 96 48)
      , flips = flipNone
      }
    body = CopyExData
      { source = SDL.Rectangle (p2  0 0) (V2 96 96)
      , destination = SDL.Rectangle (p2 (x - 48) (y - 48)) (V2 96 96)
      , flips = flipNone
      }
    frontFeet = CopyExData
      { source = SDL.Rectangle (p2 192 32) (V2 64 32)
      , destination = SDL.Rectangle (p2 (x - 36) y) (V2 96 48)
      , flips = flipNone
      }
    leftEye = CopyExData
      { source = SDL.Rectangle (p2 64 96) (V2 32 32)
      , destination = SDL.Rectangle (p2 (x - 18) (y - 21)) (V2 36 36)
      , flips = flipNone
      }
    rightEye = CopyExData
      { source = SDL.Rectangle (p2 64 96) (V2 32 32)
      , destination = SDL.Rectangle (p2  (x - 6) (y - 21)) (V2 36 36)
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

  playerTexture <- loadTexture renderer "assets/player.png"
  let
    game = Game
      { player = createPlayer playerTexture
      , camera = V2 0 0
      }
    loop = do
      events <- map SDL.eventPayload <$> SDL.pollEvents
      let pressedKeys = mapMaybe toPressedKey events
      let inputs = map toInput pressedKeys

      SDL.clear renderer
      renderTee renderer playerTexture ((position $ player game) - (P $ camera game))
      SDL.present renderer

      let quit = (SDL.QuitEvent `elem` events) || (Quit `elem` inputs)
      unless quit loop
  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
