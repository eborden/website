{-# LANGUAGE LambdaCase #-}
module Interpreter.JavaScript where

import           Control.Monad.Trans
import           Control.Monad (void, when)
import           Data.IORef
import           Data.Foldable
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHCJS.Marshal
import           GHCJS.DOM.Types hiding (Rect)
import           GHCJS.DOM (currentDocument)
import           GHCJS.DOM.JSFFI.Generated.Enums
import           GHCJS.DOM.Node (appendChild)
import           GHCJS.DOM.Element (setAttribute, getAttribute, getStyle)
import           GHCJS.DOM.EventM (on, uiKeyCode)
import           GHCJS.DOM.Document (querySelector, keyUp, keyDown, createElement)
import           GHCJS.DOM.HTMLCanvasElement
import           GHCJS.DOM.CanvasRenderingContext2D
import           JavaScript.Web.AnimationFrame
import           Lens.Micro
import           Text.Read
import           Types
import           Logic (Op(..))

interpret
  :: Show a
  => (a -> Set Op -> (a, [Shape]))
  -> (a -> Int)
  -> a
  -> IO ()
interpret render scroll seed = do
  Just doc <- currentDocument
  Just body <- querySelector doc "body"
  Just frame <- querySelector doc "#content"
  ops <- newIORef mempty
  void $ initListener doc ops

  canvasOff <- createCanvas doc
  setAttribute canvasOff "style" "display:none"

  canvasOn <- createCanvas doc
  setAttribute canvasOn "id" "landscape"

  void . appendChild body . Just $ toElement canvasOff
  void . appendChild body . Just $ toElement canvasOn

  ctxOff <- CanvasRenderingContext2D <$> getContext canvasOff "2d"
  ctxOn <- CanvasRenderingContext2D <$> getContext canvasOn "2d"

  let go last state = do
        currentOps <- readIORef ops
        let (newState, shapes) = render state currentOps
        when (last /= shapes) $ do
          forM_ shapes $ interpretShape ctxOff
          drawImageFromCanvas ctxOn (Just canvasOff) 0 0
          updateScroll frame $ scroll state
        void $ waitForAnimationFrame
        go shapes newState
  go [] seed

-- There is definitily a better way to set styles...
updateScroll :: Element -> Int -> IO ()
updateScroll el i = do
  rawStyle <- getAttribute el "style"
  original <- case rawStyle of
    Nothing -> pure 0
    Just x -> do
      case readEither . reverse . drop 2 . reverse $ drop 6 x of
        Left _ -> pure 0
        Right parsed -> pure parsed
  setAttribute el "style" $ "left: " <> show (original + i) <> "px"

createCanvas :: (MonadIO m, IsDocument self) => self -> m HTMLCanvasElement
createCanvas doc = do
  Just canvas <- fmap castToHTMLCanvasElement <$> createElement doc (Just "canvas")
  setWidth canvas 800
  setHeight canvas 800
  pure canvas

interpretShape :: CanvasRenderingContext2D -> Shape -> IO ()
interpretShape ctx = \case
  Fill r g b s -> do
    x <- toJSVal $ "rgb(" ++ show r ++ "," ++ show g  ++ ","++ show b ++ ")"
    setFillStyle ctx . Just $ CanvasStyle x
    interpretShape ctx s
    fill ctx CanvasWindingRuleNonzero
  RoundedRect x y r -> do
    let rad = fromIntegral r
    beginPath ctx
    moveTo ctx (x^.xp + rad) (x^.yp)
    lineTo ctx (y^.xp - rad) (x^.yp)
    quadraticCurveTo ctx (y^.xp) (x^.yp) (y^.xp) (x^.yp + rad)

    lineTo ctx (y^.xp) (y^.yp - rad)
    quadraticCurveTo ctx (y^.xp) (y^.yp) (y^.xp - rad) (y^.yp)

    lineTo ctx (x^.xp + rad) (y^.yp)
    quadraticCurveTo ctx (x^.xp) (y^.yp) (x^.xp) (y^.yp - rad)

    lineTo ctx (x^.xp) (x^.yp + rad)
    quadraticCurveTo ctx (x^.xp) (x^.yp) (x^.xp + rad) (x^.yp)

    closePath ctx
  BezierCurve x y z -> do
    beginPath ctx
    bezierCurveTo ctx (x^.xp) (x^.yp) (y^.xp) (y^.yp) (z^.xp) (z^.yp)
    closePath ctx
  Rect x y ->
    fillRect ctx (x^.xp) (x^.yp) (y^.xp - x^.xp) (y^.yp - x^.yp)

initListener :: Document -> IORef (Set Op) -> IO (IO ())
initListener doc ref = do
  cancelDown <- on doc keyDown $ do
    op <- keyCodeToOp <$> uiKeyCode
    forM_ op $ \x ->
      liftIO $ modifyIORef' ref $ Set.insert x
  cancelUp <- on doc keyUp $ do
    op <- keyCodeToOp <$> uiKeyCode
    forM_ op $ \x ->
      liftIO $ modifyIORef' ref $ Set.delete x
  pure $ cancelDown >> cancelUp

keyCodeToOp :: Int -> Maybe Op
keyCodeToOp = \case
  37 -> Just LeftOp
  38 -> Just UpOp
  39 -> Just RightOp
  40 -> Just DownOp
  _ -> Nothing

