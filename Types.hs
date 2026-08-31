module Types where

data Player
    = Black
    | White
    deriving (Show, Eq)

type Point = (Int, Int)