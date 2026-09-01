{-# LANGUAGE OverloadedLists #-}

module Rules where

-- If there are no lists, then
-- import Prelude hiding (filter, map)

import Data.Bool (bool)
import Control.Monad (liftM2)
import Control.Applicative (liftA2, liftA3)

-- Import set specific functions
import Data.Set (Set, unions, difference, member, notMember)
import qualified Data.Set as Set

import Types

blank :: Position -- Defines a blank position
blank = const Empty

points :: Int -> [Point] -- Gets array of points
points size = liftA2 (flip (,)) [1 .. size] [1 .. size]

setPoint :: Point -> Colour -> Position -> Position -- Mark a position
setPoint target mark board = liftA3 bool board (const mark) (target ==)

inside :: Board -> Point -> Bool -- Checks if a point is inside a board
inside board (x, y) = valid x && valid y
  where
    valid :: Int -> Bool
    valid = liftM2 (&&) (1 <=) (<= size board)

neighbours :: Board -> Point -> Set Point -- Get neighbours of a point
neighbours board (x, y) = Set.filter (inside board) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

string :: Board -> Point -> Group -- Get connected stones via flood fill
string board point = unions $ expand [point] []
  where
    same :: Point -> Bool -- Check if 
    same cand = position board point == position board cand

    expand :: Set Point -> Set Point -> [Set Point] -- Increase flood by a layer
    expand curr prev = curr : bool (expand next curr) [] (null next)
      where
        next :: Set Point
        next = Set.filter same (foldMap (neighbours board) curr) `difference` prev

liberties :: Board -> Group -> Set Point -- Identify liberties of a group
liberties board = Set.filter vacant . foldMap (neighbours board)
  where
    vacant :: Point -> Bool
    vacant = liftM2 (==) (position board) blank

clear :: Board -> Set Point -> Board -- Clear selected groups from board
clear board points = update remove board
  where
    remove :: Position -> Position
    remove pos = liftA3 bool pos blank (`member` captured)

    groups :: Set Group
    groups = Set.map (string board) points

    captured :: Set Point
    captured = unions $ Set.filter (null . liberties board) groups

move :: Board -> Player -> Point -> Board -- Execute a full turn
move board player point = clear (clear placed opponents) [point]
  where
    placed :: Board -- Board with stone at desired point
    placed = update (setPoint point $ Stone player) board

    opponents :: Set Point -- Get neighbouring enemy stones
    opponents = Set.filter enemy $ neighbours placed point

    enemy :: Point -> Bool -- Identify enemy stones
    enemy = (`notMember` [Empty, Stone player]) . position placed