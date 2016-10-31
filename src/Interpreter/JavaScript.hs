{-# LANGUAGE LambdaCase #-}
module Interpreter.JavaScript where

import           Control.Monad.Trans
import           Control.Monad (void, when)
import           Data.IORef
import           Data.Foldable
import           Data.Maybe (catMaybes, fromJust)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHCJS.Marshal
import           GHCJS.DOM.Types hiding (Rect)
import           GHCJS.DOM (currentDocument, currentWindow)
import           GHCJS.DOM.JSFFI.Generated.Enums
import           GHCJS.DOM.Node (appendChild)
import qualified GHCJS.DOM.TouchEvent as Touch
import qualified GHCJS.DOM.TouchList as Touch
import qualified GHCJS.DOM.Touch as Touch
import           GHCJS.DOM.NodeList (item, getLength)
import           GHCJS.DOM.Element (setAttribute, getAttribute)
import           GHCJS.DOM.EventM (on, uiKeyCode, mouseClientX, mouseClientY, EventM, event)
import           GHCJS.DOM.Window (getInnerWidth, getInnerHeight)
import           GHCJS.DOM.Document (querySelector, querySelectorAll, keyUp, keyDown, mouseDown, mouseUp, touchStart, touchMove, touchEnd, createElement)
import           GHCJS.DOM.HTMLCanvasElement
import           GHCJS.DOM.CanvasRenderingContext2D
import           JavaScript.Web.AnimationFrame
import           Lens.Micro
import           Text.Read
import           Types
import           Logic (Op(..))

getSize :: IO (Int, Int)
getSize = do
  Just window <- currentWindow
  (,) <$> getInnerWidth window <*> getInnerHeight window

interpret
  :: Show a
  => (a -> Set Op -> (a, [Shape]))
  -> (a -> Int)
  -> ((Int, Int) -> a -> a)
  -> a
  -> IO ()
interpret render scroll setSize seed = do
  Just doc <- currentDocument
  Just body <- querySelector doc "body"
  Just frame <- querySelector doc "#frame"
  Just content <- querySelector doc "#content"
  Just articles <- traverse nodeListToList =<< querySelectorAll doc "#content article"
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

  let go lastSize last state = do
        currentOps <- readIORef ops

        size <- getSize

        updateScroll content $ scroll state

        let (newState, shapes) = render (setSize size state) currentOps

        when (lastSize /= size) $ do
          mapM_ (resizeCanvas $ size) [canvasOn, canvasOff]
          setAttribute frame "style" $ widthHeightCSS size
          forM_ articles
            $ \el -> setAttribute el "style" $ widthHeightCSS size <> marginCSS (snd size)

        when (last /= shapes) $ do
          forM_ shapes $ interpretShape ctxOff
          drawImageFromCanvas ctxOn (Just canvasOff) 0 0

        void $ waitForAnimationFrame
        go size shapes newState
  go (0, 0) [] seed

widthHeightCSS :: (Int, Int) -> String
widthHeightCSS (w, h) = 
     "width: " <> show w <> "px;"
  <> "height: " <> show h <> "px;"

marginCSS :: Int -> String
marginCSS h = "margin-top: " <> show (h `div` 6) <> "px;"

nodeListToList :: MonadIO m => NodeList -> m [Element]
nodeListToList nl = do
  l <- getLength nl
  fmap (Element . unNode) . catMaybes <$> mapM (item nl) [0..l - 1]

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
  pure canvas

resizeCanvas ::  (MonadIO m) => (Int, Int) -> HTMLCanvasElement -> m ()
resizeCanvas (w, h) canvas =
  setWidth canvas w >> setHeight canvas h

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

  cancelMouseDown <- on doc mouseDown $ onClick ref
  cancelMouseUp <- on doc mouseUp $ offClick ref
  cancelTouchStart <- on doc touchStart $ onTouch ref
  cancelTouchMove <- on doc touchMove $ offClick ref >> onTouch ref
  cancelTouchEnd <- on doc touchEnd $ offClick ref

  pure $ cancelDown >> cancelUp
      >> cancelMouseDown >> cancelMouseUp
      >> cancelTouchStart >> cancelTouchMove >> cancelTouchEnd

onClick :: IsMouseEvent e => IORef (Set Op) -> EventM t e ()
onClick ref = do
  dim <- (,) <$> mouseClientX <*> mouseClientY
  calcDirection ref dim

onTouch :: IORef (Set Op) -> EventM t TouchEvent ()
onTouch ref = do
  Just t <- flip Touch.item 0 . fromJust =<< Touch.getTouches =<< event
  x <- Touch.getClientX t
  y <- Touch.getClientY t
  calcDirection ref (x, y)

calcDirection :: MonadIO m => IORef (Set Op) -> (Int, Int) -> m ()
calcDirection ref (x, y) = do
  (w, h) <- liftIO getSize
  let horiz = if w `div` 2 > x
        then LeftOp
        else RightOp
      vert = if h `div` 2 > y
        then UpOp
        else NoOp
  liftIO $ modifyIORef' ref $ \s -> foldr Set.insert s [vert, horiz]

offClick :: MonadIO m => IORef (Set Op) -> m ()
offClick ref = liftIO $ modifyIORef' ref $ \x -> foldr Set.delete x [UpOp, LeftOp, RightOp, DownOp]

keyCodeToOp :: Int -> Maybe Op
keyCodeToOp = \case
  37 -> Just LeftOp
  38 -> Just UpOp
  39 -> Just RightOp
  40 -> Just DownOp
  _ -> Nothing

