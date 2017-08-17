{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module TileMap
    ( readTileMap
    , TileMap(..)
    ) where

import qualified SDL
import qualified Tile
import Tile (Tile)

data TileMap = TileMap
  { texture :: SDL.Texture
  , width :: !Int
  , height :: !Int
  , tiles :: ![Tile]
  }

instance Show TileMap where
  show TileMap{width, height, tiles} =
    "TileMap(width=" ++ show width ++ ",height=" ++ show height ++ ",tiles=" ++ show tiles ++ ")"

readNumbers :: String -> [Int]
readNumbers = map read . words

readLines :: String -> [[Int]]
readLines text = map readNumbers $ lines text

readTileMap :: String -> TileMap
readTileMap text =
  let
    rows = readLines text
    width = length $ head rows
    height = length rows
  in
    TileMap
      { texture = undefined
      , width
      , height
      , tiles = toEnum <$> concat rows
      }
