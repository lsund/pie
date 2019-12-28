module Nat where

import Text.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec

import Data

add1 :: PieParser () Expression
add1 = between (char '(') (char ')') nat'
  where
    nat' = do
      _ <- string "add1 "
      Add1 <$> (zero <|> add1)

zero :: PieParser () Expression
zero = do
  _ <- string "zero"
  return Zero

nat :: PieParser () Expression
nat = zero <|> add1
