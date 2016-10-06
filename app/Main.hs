module Main where

import Data.Foldable
import Interpreter.JavaScript
import Logic

main :: IO ()
main = do
  interpret step
    $ Scene
      []
      defaultUser
      []
      []
      800 800 4 1
  where
  step x op = let n = nextTick (toList op) x in (n, drawScene x)
