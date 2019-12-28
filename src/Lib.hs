module Lib where

import Text.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec

import Data
import Nat

notComment :: Expression -> Bool
notComment (Comment _) = False
notComment _ = True

comment :: PieParser () Expression
comment = do
  _ <- char '#'
  _ <- char '_'
  Comment <$> expression

atom :: PieParser () Expression
atom = do
  x <- char '\''
  y <- many1 letter
  return $ Atom $ x : y

eliminator :: PieParser () Expression
eliminator = between (char '(') (char ')') (try cdr <|> try car)

constructor :: PieParser () Expression
constructor = between (char '(') (char ')') cons

cons :: PieParser () Expression
cons = do
  _ <- string "cons "
  [x, y] <- sepBy expression space
  return $ Pair x y

cdr :: PieParser () Expression
cdr = do
  _ <- string "cdr "
  (Pair _ y) <- expression
  return y

car :: PieParser () Expression
car = do
  _ <- string "car "
  (Pair x _) <- expression
  return x

expression :: PieParser () Expression
expression =
  try atom <|> try comment <|> try eliminator <|> try constructor <|> try nat

expressions :: PieParser () [Expression]
expressions = sepBy expression newline

parseFile :: String -> IO (Either ParseError [Expression])
parseFile = parseFromFile expressions

go :: IO ()
go = do
  parseResult <- parseFile "example.pie"
  case parseResult of
    Right result -> print (filter notComment result)
    Left emsg -> print emsg
