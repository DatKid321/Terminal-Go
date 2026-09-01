module Types where

import Data.Set (Set)
import qualified Data.Set as Set

data Player
    = Black
    | White
    deriving (Show, Eq, Ord, Enum)

type Point = (Int, Int)

type Group = Set Point

data Colour
    = Empty
    | Stone Player
    deriving (Show, Eq, Ord)

type Position = Point -> Colour

data Board = Board
    { size     :: Int
    , position :: Position
    }

update :: (Position -> Position) -> Board -> Board
update f board = board { position = f $ position board }