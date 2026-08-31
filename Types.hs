module Types where

data Player
    = Black
    | White
    deriving (Show, Eq, Enum)

type Point = (Int, Int)

data Colour
    = Empty
    | Stone Player
    deriving (Show, Eq)

type Board = Point -> Colour