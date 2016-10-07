module Main where

import Data.Foldable
import Interpreter.JavaScript
import Logic
import Types

main :: IO ()
main = do
  interpret step
    $ Scene
      [defaultCloud]
      defaultUser
      []
      [defaultHill]
      800 800 1 1
  where
  step x op =
    let n = nextTick (toList op) x
    in (n, drawScene x)
