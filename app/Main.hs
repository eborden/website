{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List (unfoldr)
import Data.Monoid
import Data.Maybe
import Data.Foldable
import Interpreter.JavaScript
import Logic
import Types
import System.Random
import Lens.Micro

main :: IO ()
main = do
  gen <- newStdGen
  gen' <- newStdGen
  gen'' <- newStdGen
  interpret step scroll
    $ Scene
      (clouds gen <> leafs gen')
      defaultUser
      []
      (clouds gen' <> hills gen'' <> trees gen)
      800 800 0.3 0.4
  where
  step x op =
    let n = nextTick (toList op) x
    in (n, drawScene x)
  scroll Scene{..} = negate . round . fromJust $ character^?velocity.xv
  clouds = take 5 . unfoldr (Just . randomCloud)
  hills = take 10 . unfoldr (Just . randomHill)
  trees = take 15 . unfoldr (Just . randomTree)
  leafs = take 30 . unfoldr (Just . randomLeaf)
