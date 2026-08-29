module Types where

data Player
  = Black
  | White
  deriving (Show, Eq)

type Coord = (Int, Int)