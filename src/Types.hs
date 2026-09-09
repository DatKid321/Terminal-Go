module Types where

import Data.Array (Array)
import Data.Set (Set)

data Player
    = Black
    | White
    deriving (Show, Eq, Ord, Enum)

type Point = (Int, Int)

type Group = Set Point

type Colour = Maybe Player

newtype Position = Position 
    { colours :: Array Point Colour 
    }
    deriving (Eq, Show, Ord)

type History = [Position]

data Turn
    = Pass
    | Move Point
    deriving (Show, Eq)

data Illegal
    = Outside
    | Occupied
    | Superko
    | Suicide
    deriving (Show, Eq)

data Rules = Rules -- Chinese/Japanese/ect preset
    { size :: Int
    , komi :: Double
    }