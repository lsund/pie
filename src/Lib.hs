module Lib where

import Data.Functor.Identity
import Text.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec
import Text.Parsec.Token

type PieParser u = ParsecT String u Identity

data Expression
  = Atom String
  | Comment Expression
  | Pair Expression Expression
  deriving (Show)

comment :: PieParser () Expression
comment = do
  _ <- char '#'
  _ <- char '_'
  x <- expression
  return $ Comment x

atom :: PieParser () Expression
atom = do
  x <- char '\''
  y <- many1 letter
  return $ Atom $ x : y

cons :: PieParser () Expression
cons = do
  _ <- string "cons "
  [x, y] <- sepBy atom space
  return $ Pair x y

cdr :: PieParser () Expression
cdr = do
  _ <- string "cdr "
  (Pair _ y) <- invocation
  return y

car :: PieParser () Expression
car = do
  _ <- string "car "
  (Pair x _) <- invocation
  return x

invocation :: PieParser () Expression
invocation = between (char '(') (char ')') (try cons <|> try cdr <|> try car)

expression :: PieParser () Expression
expression = atom <|> comment <|> invocation

expressions :: PieParser () [Expression]
expressions = sepBy expression newline

parseFile :: String -> IO (Either ParseError [Expression])
parseFile = parseFromFile expressions
