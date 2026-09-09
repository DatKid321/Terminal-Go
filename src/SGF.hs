module SGF where

import Data.Function (on)
import Data.Bool (bool)
import Data.Char (ord)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Text.Read (readMaybe)
import Data.Foldable (asum)

import Rules
import Parser
import Types

-- Helpers

root :: Tree -> [Property]
root (Tree (Node properties : _) _) = properties
root _                              = []

decodePoint :: Value -> Maybe Point
decodePoint [x, y] = Just $ on (,) (subtract 96 . ord) x y
decodePoint _      = Nothing

-- Move decoding

decodeTurn :: Property -> Maybe (Player, Turn)
decodeTurn (Property "B" [""])    = Just (Black, Pass)
decodeTurn (Property "W" [""])    = Just (White, Pass)
decodeTurn (Property "B" [value]) = (Black,) . Move <$> decodePoint value
decodeTurn (Property "W" [value]) = (White,) . Move <$> decodePoint value
decodeTurn _                      = Nothing

decodeNode :: Node -> [(Player, Turn)]
decodeNode (Node properties) = mapMaybe decodeTurn properties

decodeTree :: Tree -> [(Player, Turn)]
decodeTree (Tree nodes trees) = foldMap decodeNode nodes ++ foldMap decodeTree (listToMaybe trees) -- Only follows first variation

-- Property decoding

decodeValue :: Read a => Ident -> a -> [Property] -> a
decodeValue ident preset = fromMaybe preset . asum . map value
  where
    value :: Read a => Property -> Maybe a
    value (Property name [text])
        | name == ident = readMaybe text
    value _             = Nothing

decodeSize :: [Property] -> Int
decodeSize = decodeValue "SZ" 19

decodeKomi :: [Property] -> Double
decodeKomi = decodeValue "KM" 0

decodeSetup :: [Property] -> Position -> Position
decodeSetup = flip $ foldr apply
  where
    apply :: Property -> Position -> Position
    apply (Property "AB" values) = set (Just Black) values
    apply (Property "AW" values) = set (Just White) values
    apply (Property "AE" values) = set Nothing values
    apply _                      = id

    set :: Colour -> [Value] -> Position -> Position
    set mark = flip (foldr (`setPoint` mark)) . mapMaybe decodePoint

-- Rules decoding

decodeRules :: Tree -> Rules
decodeRules = rules . root
  where
    rules :: [Property] -> Rules
    rules = Rules
        <$> decodeSize 
        <*> decodeKomi