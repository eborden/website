module Main where

import Data.List (unfoldr)
import Data.Monoid
import Data.Foldable
import Interpreter.JavaScript
import Logic
import Types
import System.Random

main :: IO ()
main = do
  gen <- newStdGen
  gen' <- newStdGen
  gen'' <- newStdGen
  interpret step
    $ Scene
      (clouds gen <> leafs gen')
      defaultUser
      []
      (clouds gen' <> hills gen'' <> trees gen)
      800 800 1 1
  where
  step x op =
    let n = nextTick (toList op) x
    in (n, drawScene x)
  clouds = take 10 . unfoldr (Just . randomCloud)
  hills = take 15 . unfoldr (Just . randomHill)
  trees = take 30 . unfoldr (Just . randomTree)
  leafs = take 30 . unfoldr (Just . randomLeaf)
