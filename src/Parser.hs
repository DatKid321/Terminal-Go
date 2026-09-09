module Parser where

import Control.Applicative (Alternative (..))
import Data.Char (isSpace, isAsciiUpper)
import Data.List (uncons)
import Control.Monad (mfilter)
import Control.Monad.Trans.State (StateT(..), runStateT)

type Ident = String
type Value = String

data Property = Property Ident [Value]
    deriving (Eq, Show)

newtype Node = Node [Property]
    deriving (Eq, Show)

data Tree = Tree [Node] [Tree]
    deriving (Eq, Show)

type Collection = [Tree]

type Parser = StateT String Maybe

parse :: Parser a -> String -> Maybe (a, String)
parse = runStateT

item :: Parser Char
item = StateT uncons

satisfy :: (Char -> Bool) -> Parser Char
satisfy = flip mfilter item

is :: Char -> Parser Char
is = satisfy . (==)

isNot :: Char -> Parser Char
isNot = satisfy . (/=)

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = many space

tok :: Parser a -> Parser a
tok = (<* spaces)

charTok :: Char -> Parser Char
charTok = tok . is

escape :: Parser Char
escape = is '\\' *> item

valueChar :: Parser Char
valueChar = escape <|> isNot ']'

parseIdent :: Parser Ident
parseIdent = tok $ some $ satisfy isAsciiUpper

parseValue :: Parser Value
parseValue = tok $ is '[' *> many valueChar <* is ']'

parseProperty :: Parser Property
parseProperty = Property <$> parseIdent <*> some parseValue

parseNode :: Parser Node
parseNode = Node <$ charTok ';' <*> many parseProperty

parseTree :: Parser Tree
parseTree = Tree <$ charTok '(' <*> some parseNode <*> many parseTree <* charTok ')'

parseCollection :: Parser Collection
parseCollection = spaces *> some parseTree