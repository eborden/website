{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Logic
  (module Logic, defaultUser
  ) where

import Data.Monoid
import Lens.Micro
import Types

data Op
  = UpOp
  | RightOp
  | DownOp
  | LeftOp
  | NoOp

data Scene
  = Scene
  { foreground   :: [Sprite]
  , character    :: Sprite
  , stage        :: [Sprite]
  , background   :: [Sprite]
  , screenHeight :: Int
  , screenWidth  :: Int
  , gravity      :: Int
  , friction     :: Int
  } deriving (Show, Eq)

addHeight :: Sprite -> Double -> Int -> Int
addHeight s m x = ((s^.bounds&height) *~ m) + x

addWidth :: Sprite -> Double -> Int -> Int
addWidth s m x = ((s^.bounds&width) *~ m) + x

draw :: Sprite -> [Shape]
draw s = case s of
  User b _ (Velocity x _)
    -- front
    | x == 0    -> [ Fill 50 50 50
                   $ RoundedRect (s^.topLeft) (s^.bottomRight) 10 ]
    -- right
    | x > 0     -> [ Fill 50 50 50
                   $ RoundedRect
                      (s^.topLeft)
                      (s^.bottomRight & right b %~ addWidth s (-0.1))
                      10]
    -- left
    | otherwise -> [ Fill 50 50 50
                   $ RoundedRect
                      (s^.topLeft & left %~ addWidth s 0.1)
                      (s^.bottomRight)
                      10]

  Leaf _ _ _ ->
    [ Fill 255 0 0 $ RoundedRect (s^.topLeft) (s^.bottomRight) 10 ]

  Cloud b _ _ ->
    fmap (Fill 255 255 255)
      [ RoundedRect (s^.bottomLeft & top b %~ addHeight s 0.4)
      {- top -}     (s^.bottomRight)
                    10
      , RoundedRect (s^.topLeft & left %~ addWidth s 0.2)
      {- bottom -}  (s^.bottomRight & right b %~ addWidth s (-0.2))
                    10
      ]

  Tree b _ ->
    [ Fill 139 69 19 $ Rect (s^.topLeft & left %~ addWidth s 0.3)
                          (s^.bottomRight & right b %~ addWidth s (-0.4))
    , Fill 34 139 34 $ RoundedRect (s^.topLeft)
                                (s^.bottomRight & bottom %~ addHeight s (-0.2))
                                10
    ]

  Hill b p ->
    [ Fill 255 255 0 $ BezierCurve (s^.bottomLeft)
                                  (s^.topLeft & left .~ fst (centerPoint b p))
                                  (s^.bottomRight)
    ]

collides :: Sprite -> Sprite -> Bool
collides sa sb 
  | sa^.bottomRight > sb^.topLeft && sb^.bottomRight < sa^.topLeft = False
  | otherwise = True

applyGravity :: Int -> Int -> Sprite -> Sprite
applyGravity constant lowerBound = \case
  User d p v
    | lowerBound == p^.bottom -> User d p $ ground v
    | otherwise               -> User d p $ gravityVeloc constant v
  x -> x

gravityVeloc :: Int -> Velocity -> Velocity
gravityVeloc constant (Velocity x y) = Velocity x $ y + constant

applyFriction :: Int -> Sprite -> Sprite
applyFriction constant s = case s of
  User _ _ _ -> s & velocity . xv %~ frict
  x -> x
  where
  frict x 
    | x > 0     = max 0 $ x - constant
    | x < 0     = min 0 $ x + constant
    | otherwise = x

ground :: Velocity -> Velocity
ground (Velocity x _) = Velocity x 0

wall :: Velocity -> Velocity
wall (Velocity _ y) = Velocity 0 y

advanceSprite :: Velocity -> Sprite -> Sprite
advanceSprite characterV s = case s of
  User _ _ _  -> s & position %~ applyVeloc (wall characterV)
  _           -> s & position %~ applyVeloc (ground characterV)
                               . maybe id applyVeloc (s ^? velocity)

applyVeloc :: Velocity -> Position -> Position
applyVeloc (Velocity vx vy) (Position x y) = Position (x + vx) (y + vy)

operationVelocity :: Op -> Velocity -> Velocity
operationVelocity op v@(Velocity x y) =
  case op of
    UpOp -> Velocity x $ min (y - 6) 25
    RightOp -> (Velocity (min (x + 2) 25) y)
    DownOp -> v
    LeftOp -> (Velocity (max (x - 2) (-25)) y)
    NoOp -> v

nextTick :: [Op] -> Scene -> Scene
nextTick op scene =
  scene { foreground = fmap (next []) $ foreground scene
        , character = next s c
        , stage = fmap (next [c]) s
        , background = fmap (next []) $ background scene
        }
  where
  c = character scene & velocity %~ foldMap operationVelocity op 
  s = stage scene
  User _ _ charV = c

  next :: [Sprite] -> Sprite -> Sprite
  next xs x =
    if any (==True) (fmap (collides x) xs) then
      x
    else
      advanceSprite charV
        . applyFriction (friction scene)
        $ applyGravity (gravity scene) (screenHeight scene) x

drawScene :: Scene -> [Shape]
drawScene Scene{..} =
  [Fill 135 206 250 $ Rect (Position 0 0) (Position screenWidth screenHeight)]
    <> draw' background
    <> draw' stage
    <> draw character
    <> draw' foreground
  where
  draw' = concatMap draw
{-
main = do
  {-
  let s = Scene [] (User) [] []
      shapes = nextTick . drawScene
      out = shapes s
  -}
  mainWidget $ do
    el "article" $ do
      el "h1" $ text "Evan Rutledge Borden"
      el "h2" $ text "Application Developer"
      el "p" $ text "Use your keyboard's arrow keys to fly and explore."
    el "article" $ do
      el "h1" $ text "Effective Design"
      el "div" $ do
        el "ul" $ do
          el "li" $ text "Understands trust"
          el "li" $ text "Doesn't get in the way"
          el "li" $ text "Accomplishes its task swiftly"
          el "li" $ text "Leaves room for creativity"
          el "li" $ text "Is less"
    el "article" $ do
      el "h1" $ text "Ask Me Anything"
      el "form" $ do
        el "p" $ do
          el "label" $ text "My Email"
          el "span" $ text "evan@evan-borden.com"
        el "p" $ do
          el "label" $ text "Your Email"
          textInput
        el "p" $ do
          el "label" $ text "Message"
          textArea def
          text ""
          -}

{- JavaScript
-- rectangle
  ctx.fillStyle = '#897723';
  ctx.fillRect(this.x + (this.w * 0.3), this.y + (this.h * 0.8), this.w * 0.4, this.h * 0.2);

-- round rectangle
  // var radius
  ctx.fillStyle = fill;
  ctx.beginPath();
  ctx.moveTo(x + radius, y);
  ctx.lineTo(x + width - radius, y);
  ctx.quadraticCurveTo(x + width, y, x + width, y + radius);

  ctx.lineTo(x + width, y + height - radius);
  ctx.quadraticCurveTo(x + width, y + height, x + width - radius, y + height);

  ctx.lineTo(x + radius, y + height);
  ctx.quadraticCurveTo(x, y + height, x, y + height - radius);

  ctx.lineTo(x, y + radius);
  ctx.quadraticCurveTo(x, y, x + radius, y);
  ctx.closePath();
  ctx.fill();

-- Bezier Curve
  ctx.fillStyle = this.color;
  ctx.beginPath();
  ctx.bezierCurveTo(this.x, this.y, this.x + (this.w /2), this.y - this.h, this.x + this.w, this.y );
  ctx.closePath();
  ctx.fill();
-}
