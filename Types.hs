module Types where

data Player
    = Black
    | White
    deriving (Show, Eq, Enum)

type Point = (Int, Int)

type Group = [Point]

data Colour
    = Empty
    | Stone Player
    deriving (Show, Eq)

type Position = Point -> Colour

data Board = Board
    { size     :: Int
    , position :: Position
    }

update :: (Position -> Position) -> Board -> Board
update f board = board { position = f $ position board }