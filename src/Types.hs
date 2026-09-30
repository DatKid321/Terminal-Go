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

type History = [(Position, Maybe Player)]

data Turn
    = Pass
    | Move Point
    deriving (Show, Eq)

data Illegal
    = Outside
    | Occupied
    | Suicide
    | Repetition
    deriving (Show, Eq)

-- Rule variations

data Ko
    = Simple
    | Positional
    | Situational
    | Natural
    deriving (Eq, Show)

type Suicide = Bool -- True: suicide allowed

data Scoring
    = Area
    | Territory
    deriving (Eq, Show)

data Ruleset = Ruleset
    { ko      :: Ko
    , suicide :: Bool
    , scoring :: Scoring
    }

chinese, japanese, korean, aga, newZealand, trompTaylor, british, french :: Ruleset
chinese     = Ruleset Positional  False Area
japanese    = Ruleset Simple      False Territory
korean      = Ruleset Simple      False Territory
aga         = Ruleset Situational False Area
newZealand  = Ruleset Situational True  Area
trompTaylor = Ruleset Positional  True  Area
british     = Ruleset Natural     False Territory
french      = Ruleset Natural     False Area

data Rules = Rules
    { size    :: Int
    , komi    :: Double
    , ruleset :: Ruleset
    }