module Parser where

import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isDigit, isLower, isSpace, isUpper, ord)
import Data.List (uncons, find)
import Data.Maybe (maybeToList)
import Control.Monad.Trans.State
import Data.Functor

type Parser a = StateT String Maybe a

parse :: Parser a -> String -> Maybe (a,String)
parse = runStateT

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = StateT $ find (p . fst) . maybeToList . uncons

is :: Char -> Parser Char
is = satisfy . (==)

space :: Parser Char
space = satisfy isSpace

alpha :: Parser Char
alpha = satisfy isAlpha

spaces :: Parser String
spaces = many space

tok :: Parser a -> Parser a
tok = (<* spaces)

charTok :: Char -> Parser Char
charTok = tok . is

data Player
  = Black
  | White
  deriving (Show, Eq)

type Coord = (Int, Int)

parseColour :: Parser Player
parseColour = (is 'B' $> Black) <|> (is 'W' $> White)

parseCoords :: Parser Coord
parseCoords = (,) <$> (charTok '[' *> alphaIndex) <*> (alphaIndex <* charTok ']')
    where
        alphaIndex = fmap (subtract 96 . ord) alpha

parseMove :: Parser (Player, Coord)
parseMove = (,) <$> (charTok ';' *> parseColour) <*> parseCoords

parseGame :: Parser [(Player, Coord)]
parseGame = many parseMove