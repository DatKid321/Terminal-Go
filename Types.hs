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

type Colouring = [Colour]

type Position = Point -> Colour

type History = [Position]

data Illegal
    = Occupied
    | Superko
    | Suicide
    deriving (Show, Eq)

data Rules = Rules -- Chinese/Japanese preset
    { size :: Int,
      more :: ()
    }