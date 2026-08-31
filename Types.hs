module Types where

data Player
    = Black
    | White
    deriving (Show, Eq, Enum)

type Point = (Int, Int)