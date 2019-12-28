module Pair where

import Text.ParserCombinators.Parsec
import Data

import Lib (expression)

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
