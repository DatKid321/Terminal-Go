module Rules where

import Data.List (nub, (\\))
import Data.Bool (bool)
import Control.Monad (join, liftM2)
import Control.Applicative (liftA3)

import Types

blank :: Position -- Defines a blank position
blank = const Empty

setPoint :: Point -> Colour -> Position -> Position -- Mark a position
setPoint target mark board = liftA3 bool board (const mark) (target ==)

inside :: Board -> Point -> Bool -- Checks if a point is inside a board
inside board (x, y) = all (liftA2 (&&) (1 <=) (<= size board)) [x, y]

neighbours :: Board -> Point -> [Point] -- Get neighbours of a point
neighbours board (x, y) = filter (inside board) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

string :: Board -> Point -> Group -- Get connected stones via flood fill
string board point = concat $ expand [point] []
  where
    same :: Point -> Bool
    same cand = position board point == position board cand

    expand :: [Point] -> [Point] -> [[Point]]
    expand curr prev = curr : bool (expand next curr) [] (null next)
      where
        next :: [Point]
        next = nub (filter same . neighbours board =<< curr) \\ prev

liberties :: Board -> Group -> [Point] -- Identify liberties of a group
liberties board = nub . filter empty . (neighbours board =<<)
  where
    empty :: Point -> Bool
    empty = liftM2 (==) (position board) blank

clear :: Board -> [Point] -> Board -- Clear selected groups from board
clear board points = update remove board
  where
    remove :: Position -> Position
    remove pos = liftA3 bool pos blank (`elem` captured)

    captured :: [Point]
    captured = join . filter (null . liberties board) . map (string board) $ points