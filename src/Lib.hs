module Lib where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim                 hiding  (try)
import Data.Functor.Identity

type PieParser u = ParsecT String u Identity

data Expression = Atom String | Comment Expression
    deriving (Show)

parseComment :: PieParser () Expression
parseComment = do
    _ <- char '#'
    _ <- char '_'
    x <- expression
    return $ Comment x

atom :: PieParser () Expression
atom = do
    x <- char '\''
    y <- many1 letter
    return $ Atom $ x : y

expression :: PieParser () Expression
expression = atom <|> parseComment

parsePie :: PieParser () [Expression]
parsePie = sepBy expression newline

parseFile :: String -> IO (Either ParseError [Expression])
parseFile = parseFromFile parsePie
