{-# LANGUAGE OverloadedLists #-}

module Rules where

-- If there are no lists, then
-- import Prelude hiding (filter, map)

import Data.Bool (bool)
import Data.Function (on)
import Data.Maybe (isJust, isNothing)
import Control.Applicative (liftA2, liftA3)

-- Import set specific functions
import Data.Set (Set, unions, difference, member, singleton)
import qualified Data.Set as Set

import Types

-- Good for partial application:
-- Functions taking only rules first are Geometric, position is not needed
-- Functions taking rules then position are Queries, fix rules and state before asking
-- Functions taking position to position are Transformative, position to position is natural

blank :: Position -- Defines a blank position
blank = const Nothing

points :: Rules -> [Point] -- Gets array of points
points rules = liftA2 (,) [1 .. size rules] [1 .. size rules]

colouring :: Rules -> Position -> Colouring -- Get a colouring of the board
colouring rules pos = map pos $ points rules

setPoint :: Point -> Colour -> Position -> Position -- Mark a position
setPoint target mark pos = liftA3 bool pos (const mark) (target ==)

inside :: Rules -> Point -> Bool -- Check point lies on board
inside rules (x, y) = valid x && valid y
  where
    valid :: Int -> Bool
    valid = liftA2 (&&) (1 <=) (<= size rules)

neighbours :: Rules -> Point -> Set Point -- Get neighbours of a point
neighbours rules (x, y) = Set.filter (inside rules) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

string :: Rules -> Position -> Point -> Group -- Get connected region containing point
string rules pos point = unions $ expand [point] []
  where
    same :: Point -> Bool -- Check if point has same colour
    same = on (==) pos point

    expand :: Set Point -> Set Point -> [Set Point] -- Expand region by a layer
    expand curr prev = curr : bool (expand next curr) [] (null next)
      where
        next :: Set Point -- Next unvisited layer
        next = Set.filter same (foldMap (neighbours rules) curr) `difference` prev

liberties :: Rules -> Position -> Group -> Set Point -- Get empty points adjacent to a group
liberties rules pos = Set.filter (isNothing . pos) . foldMap (neighbours rules)

clear :: Rules -> Set Point -> Position -> Position -- Remove groups with no liberties
clear rules points pos = liftA3 bool pos blank (`member` captured)
  where
    groups :: Set Group -- Groups containing given points
    groups = Set.map (string rules pos) points

    captured :: Set Point -- Points in groups with no liberties
    captured = unions $ Set.filter (null . liberties rules pos) groups

move :: Rules -> Player -> Point -> Position -> Position -- Place a stone
move rules player point pos = clear rules [point] cleared
  where
    placed :: Position -- Position after placing stone
    placed = setPoint point (Just player) pos

    enemy :: Point -> Bool -- Check point contains an enemy stone
    enemy = maybe False (/= player) . placed

    opponents :: Set Point -- Adjacent enemy stones
    opponents = Set.filter enemy $ neighbours rules point

    cleared :: Position -- Position after removing enemy groups
    cleared = clear rules opponents placed

play :: Rules -> Player -> Turn -> History -> Either Illegal History -- Place a stone if legal
play _     _      Pass         past@(pos : _) = Right $ pos : past -- Record pass
play rules player (Move point) past@(pos : _)
    | not $ inside rules point                = Left Outside -- Point is on board
    | isJust $ pos point                      = Left Occupied -- Point contains stone
    | colouring rules next `elem` colourings  = Left Superko -- Position has been repeated
    | otherwise                               = Right $ next : past -- Position is legal
  where
    next :: Position -- Position after move
    next = move rules player point pos

    colourings :: [Colouring] -- Previous positions
    colourings = map (colouring rules) past

ended :: Rules -> History -> Bool
ended rules (pos : prev : _) = on (==) (colouring rules) pos prev
ended _     _                = False

score :: Rules -> Position -> Player -> Int
score rules pos player = length $ filter owned $ points rules
  where
    owned :: Point -> Bool
    owned point = maybe (owners point == [player]) (== player) $ pos point

    owners :: Point -> Set Player
    owners = foldMap (foldMap singleton . pos) . foldMap (neighbours rules) . string rules pos