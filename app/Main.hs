module Main where

import Logic
import Types
import Interpreter.JavaScript

main :: IO ()
main = do
  interpreter <- interpret
  interpreter . drawScene
    . nextTick []
    . nextTick []
    . nextTick []
    . nextTick []
    . nextTick [RightOp]
    . nextTick [RightOp]
    . nextTick [RightOp]
    . nextTick [RightOp]
    . nextTick [RightOp]
    $ Scene
      []
      defaultUser
      []
      []
      800 800 4 1
