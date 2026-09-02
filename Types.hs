module Types where

import Data.Set (Set)
import qualified Data.Set as Set

data Player
    = Black
    | White
    deriving (Show, Eq, Ord, Enum)

type Point = (Int, Int)

type Group = Set Point

type Colour = Maybe Player

type Colouring = [Colour]

type Position = Point -> Colour

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
    { size :: Int,
      more :: ()
    }