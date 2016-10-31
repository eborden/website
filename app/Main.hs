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
  screen <- uncurry Bounds <$> getSize

  gen <- newStdGen
  gen' <- newStdGen
  gen'' <- newStdGen

  let clouds = take 5 . unfoldr (Just . randomCloud screen)
      hills = take 15 . unfoldr (Just . randomHill screen)
      trees = take 15 . unfoldr (Just . randomTree screen)
      leafs = take 30 . unfoldr (Just . randomLeaf screen)

  interpret step scroll size
    $ Scene
    { foreground = clouds gen <> leafs gen'
    , character = defaultUser screen
    , stage = []
    , background = clouds gen' <> hills gen'' <> trees gen
    , screenHeight = height screen
    , screenWidth = width screen
    , gravity = 0.5
    , friction = 0.4
    }
  where
  step x op =
    let n = nextTick (toList op) x
    in (n, drawScene x)
  scroll Scene{..} = negate . round . fromJust $ character^?velocity.xv
  size (w, h) s = s{screenWidth = w, screenHeight = h}
