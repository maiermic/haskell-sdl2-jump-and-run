{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when, unless)
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)
import Foreign.C.Types (CInt)
import SDL.Vect (V2(..), V4(..))
import SDL (($=))
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1280, 720)

data Input
  = None
  | MoveLeft
  | MoveRight
  | Jump
  | Restart
  | Quit
  deriving (Eq)

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
  SDL.rendererDrawColor renderer $= V4 110 132 174 maxBound

  let
    loop = do
      events <- map SDL.eventPayload <$> SDL.pollEvents
      let pressedKeys = mapMaybe toPressedKey events
      let inputs = map toInput pressedKeys

      SDL.clear renderer
      SDL.present renderer

      let quit = (SDL.QuitEvent `elem` events) || (Quit `elem` inputs)
      unless quit loop
  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
