module Lib where

import Data.Functor.Identity
import Text.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec

type PieParser u = ParsecT String u Identity

data Expression
  = Atom String
  | Comment Expression
  | Pair Expression Expression
  | Zero
  | Add1 Expression

instance Show Expression where
  show (Atom x) = x
  show (Comment _) = ""
  show (Pair x y) = "Pair[" <> show x <> "," <> show  y  <> "]"
  show Zero = show $ foldNat Zero
  show (Add1 x) = (show . foldNat . Add1) x

foldNat :: Expression -> Int
foldNat (Add1 x) = 1 + foldNat x
foldNat Zero = 0
foldNat _ = 0

notComment :: Expression -> Bool
notComment (Comment _) = False
notComment _ = True

number :: PieParser () Int
number = read <$> many1 digit

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

add1 :: PieParser () Expression
add1 =
    between (char '(') (char ')') add1'
    where
        add1' = do
            _ <- string "add1 "
            Add1 <$> (zero <|> add1)

zero :: PieParser () Expression
zero = do
    _ <- string "zero"
    return Zero

nat :: PieParser () Expression
nat =
  zero <|> add1

invocation :: PieParser () Expression
invocation = between
                (char '(')
                (char ')')
                (try cons <|> try cdr <|> try car)

expression :: PieParser () Expression
expression = try atom <|> try comment <|> try invocation <|> try nat

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
