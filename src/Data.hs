module Data where

import Data.Functor.Identity
import Text.Parsec.Prim hiding (try)

type PieParser u = ParsecT String u Identity

data Expression
  = Atom String
  | Comment Expression
  | Pair Expression Expression
  | Zero
  | Add1 Expression

foldNat :: Expression -> Int
foldNat (Add1 x) = 1 + foldNat x
foldNat Zero = 0
foldNat _ = 0

instance Show Expression where
  show (Atom x) = x
  show (Comment _) = ""
  show (Pair x y) = "Pair[" <> show x <> "," <> show  y  <> "]"
  show Zero = show $ foldNat Zero
  show (Add1 x) = (show . foldNat . Add1) x
