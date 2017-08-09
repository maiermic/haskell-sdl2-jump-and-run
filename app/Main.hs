{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Foreign.C.Types (CInt)
import SDL.Vect (V2(..), V4(..))
import SDL (($=))
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1280, 720)

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

  SDL.clear renderer
  SDL.present renderer

  threadDelay 2000000

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
