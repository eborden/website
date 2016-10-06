{-# LANGUAGE LambdaCase #-}
module Interpreter.JavaScript where

import Lens.Micro
import JavaScript.Web.Canvas
import JavaScript.Web.Canvas.Internal (Canvas(..))
import Types
import Data.Foldable
import GHCJS.Types

foreign import javascript unsafe "document.body.appendChild($1);"
  js_insert :: JSVal -> IO ()

insert :: Canvas -> IO ()
insert (Canvas x) = js_insert x

interpret :: IO ([Shape] -> IO ())
interpret = do
  canvas <- create 800 800
  insert canvas
  ctx <- getContext canvas
  pure $ \shapes -> forM_ shapes $ \x -> do
    print x
    interpretShape ctx x

interpretShape :: Context -> Shape -> IO ()
interpretShape ctx = \case
  Fill r g b s -> do
    fillStyle r g b 1 ctx
    interpretShape ctx s
    fill ctx
  RoundedRect x y r -> do
    let rad = fromIntegral r
    beginPath ctx
    moveTo (x^.xp + rad) (x^.yp) ctx
    lineTo (y^.xp - rad) (x^.yp) ctx
    quadraticCurveTo (y^.xp) (y^.yp) (y^.xp) (x^.yp + rad) ctx

    lineTo (y^.xp) (y^.yp - rad) ctx
    quadraticCurveTo (y^.xp) (y^.yp) (y^.xp - rad) (y^.yp) ctx

    lineTo (x^.xp + rad) (y^.yp) ctx
    quadraticCurveTo (x^.xp) (y^.yp) (x^.xp) (y^.yp - rad) ctx

    lineTo (x^.xp) (x^.yp + rad) ctx
    quadraticCurveTo (x^.xp) (x^.yp) (x^.xp + rad) (x^.yp) ctx

    closePath ctx
  BezierCurve x y z -> do
    beginPath ctx
    bezierCurveTo (x^.xp) (x^.yp) (y^.xp) (y^.yp) (z^.xp) (z^.yp) ctx
    closePath ctx
  Rect x y ->
    fillRect (x^.xp) (x^.yp) (y^.xp) (y^.yp) ctx
