module Main where

import Lib

main :: IO ()
main = do
  parseResult <- parseFile "example.pie"
  case parseResult of
    Right result -> print (filter notComment result)
    Left error -> print error
