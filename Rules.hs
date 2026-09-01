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

-- Functions taking only rules first are Geometric, 
-- Functions taking 

blank :: Position -- Defines a blank position
blank = const Empty

points :: Rules -> [Point] -- Gets array of points
points rules = liftA2 (flip (,)) [1 .. size rules] [1 .. size rules]

setPoint :: Point -> Colour -> Position -> Position -- Mark a position
setPoint target mark pos = liftA3 bool pos (const mark) (target ==)

inside :: Rules -> Point -> Bool -- Checks if a point is legal
inside rules (x, y) = valid x && valid y
  where
    valid :: Int -> Bool
    valid = liftM2 (&&) (1 <=) (<= size rules)

neighbours :: Rules -> Point -> Set Point -- Get neighbours of a point
neighbours rules (x, y) = Set.filter (inside rules) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

string :: Rules -> Position -> Point -> Group -- Get connected region containing point
string rules pos point = unions $ expand [point] []
  where
    same :: Point -> Bool -- Check if point has same colour
    same cand = pos point == pos cand

    expand :: Set Point -> Set Point -> [Set Point] -- Expand region by a layer
    expand curr prev = curr : bool (expand next curr) [] (null next)
      where
        next :: Set Point -- Next unvisited layer
        next = Set.filter same (foldMap (neighbours rules) curr) `difference` prev

liberties :: Rules -> Position -> Group -> Set Point -- Identify liberties of a group
liberties rules pos = Set.filter vacant . foldMap (neighbours rules)
  where
    vacant :: Point -> Bool
    vacant = liftM2 (==) pos blank

clear :: Rules -> Set Point -> Position -> Position -- Clear selected groups from board
clear rules points pos = liftA3 bool pos blank (`member` captured)
  where
    groups :: Set Group
    groups = Set.map (string rules pos) points

    captured :: Set Point
    captured = unions $ Set.filter (null . liberties rules pos) groups

move :: Rules -> Player -> Point -> Position -> Position -- Execute a move
move rules player point pos = clear rules [point] cleared
  where
    placed :: Position -- Position after placing stone
    placed = setPoint point (Stone player) pos

    opponents :: Set Point -- Adjacent enemy stones
    opponents = Set.filter enemy $ neighbours rules point

    enemy :: Point -> Bool -- Check point contains an enemy stone
    enemy = (`notMember` [Empty, Stone player]) . placed

    cleared :: Position -- Position after removing enemy groups
    cleared = clear rules opponents placed