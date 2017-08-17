module Tile
    ( Tile(..)
    ) where

import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)

data Tile
  = Air
  | Start
  | Finish
  | Unknown Int
  deriving (Eq, Show)

instance Enum Tile where
    fromEnum (Unknown tileNr) = tileNr
    fromEnum tile = fromJust . flip lookup tileTable $ tile
    toEnum tileNr = fromMaybe (Unknown tileNr) $ lookup tileNr (map swap tileTable)

tileTable =
  [ (Air, 0)
  , (Start, 78)
  , (Finish, 110)
  ]
