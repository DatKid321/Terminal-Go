module SGF where

import Data.Function (on)
import Data.Char (ord)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Text.Read (readMaybe)

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
decodeTree (Tree nodes trees) = concatMap decodeNode nodes ++ maybe [] decodeTree (listToMaybe trees) -- Only follows first variation

-- Property decoding

decodeSize :: [Property] -> Int
decodeSize = fromMaybe 19 . listToMaybe . mapMaybe size
  where
    size (Property "SZ" [value]) = readMaybe value
    size _                       = Nothing

decodeSetup :: [Property] -> Position -> Position
decodeSetup = flip $ foldr apply
  where
    apply :: Property -> Position -> Position
    apply (Property "AB" values) = set (Just Black) values
    apply (Property "AW" values) = set (Just White) values
    apply (Property "AE" values) = set Nothing values
    apply _                      = id

    set :: Colour -> [Value] -> Position -> Position
    set mark = flip $ foldr $ maybe id (`setPoint` mark) . decodePoint

-- Rules decoding

decodeRules :: Tree -> Rules
decodeRules tree = Rules
    { size = decodeSize $ root tree
    , more = ()
    }