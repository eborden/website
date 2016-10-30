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
  deriving (Show, Eq, Ord)

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

addHeight :: Sprite -> Float -> Int -> Int
addHeight s m y = ((s^.bounds&height) *~ m) + y

addWidth :: Sprite -> Float -> Int -> Int
addWidth s m x = ((s^.bounds&width) *~ m) + x

draw :: Sprite -> [Shape]
draw s = case s of
  User b _ (Velocity x _)
    -- front
    | x == 0    ->
      [ Fill 102 88 73
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s (-0.1))
          (s^.topLeft & bottom %~ addHeight s 0.2
                      & left %~ addWidth s 0.3)
          3
      , Fill 102 88 73
      $ RoundedRect
          (s^.topRight & top b %~ addHeight s (-0.1)
                       & right b %~ addWidth s (-0.3))
          (s^.topRight & bottom %~ addHeight s 0.2)
          3
      , Fill 102 88 73
      $ RoundedRect
          (s^.topLeft)
          (s^.topRight & top b %~ addHeight s 0.333)
          3
      , Fill 219 183 132
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.1
                      & left %~ addWidth s 0.1)
          (s^.topRight & top b %~ addHeight s 0.333
                       & right b %~ addWidth s (-0.1))
          3
      , Fill 102 88 73
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.333
                      & left %~ addWidth s 0.1)
          (s^.bottomRight & right b %~ addWidth s (-0.1))
          6
      ]
    -- right
    | x > 0     ->
      [ Fill 102 88 73
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s (-0.1)
                      & left %~ addWidth s 0.2)
          (s^.topLeft & bottom %~ addHeight s 0.2
                      & left %~ addWidth s 0.5)
          3
      , Fill 102 88 73
      $ RoundedRect
          (s^.topLeft)
          (s^.topRight & top b %~ addHeight s 0.333)
          3
      , Fill 219 183 132
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.1
                      & left %~ addWidth s 0.3)
          (s^.topRight & top b %~ addHeight s 0.333)
          3
      , Fill 102 88 73
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.333
                      & left %~ addWidth s 0.1)
          (s^.bottomRight & right b %~ addWidth s (-0.1))
          6
      ]
    -- left
    | otherwise ->
      [ Fill 102 88 73
      $ RoundedRect
          (s^.topRight & top b %~ addHeight s (-0.1)
                       & right b %~ addWidth s (-0.2))
          (s^.topRight & bottom %~ addHeight s 0.2
                       & right b %~ addWidth s (-0.5))
          3
      , Fill 102 88 73
      $ RoundedRect
          (s^.topLeft)
          (s^.topRight & top b %~ addHeight s 0.333)
          3
      , Fill 219 183 132
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.1)
          (s^.topRight & top b %~ addHeight s 0.333
                       & right b %~ addWidth s (-0.3))
          3
      , Fill 102 88 73
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.333
                      & left %~ addWidth s 0.1)
          (s^.bottomRight & right b %~ addWidth s (-0.1))
          6
      ]

  Leaf _ _ (r, g, b) _ ->
    [ Fill r g b $ RoundedRect (s^.topLeft) (s^.bottomRight) 10 ]

  Cloud b _ _ ->
    fmap (Fill 255 255 255)
      [ RoundedRect (s^.topLeft & top b %~ addHeight s (0.3333))
      {- bottom -}  (s^.bottomRight)
                    10
      , RoundedRect (s^.topLeft & left %~ addWidth s 0.15)
      {- top -}     (s^.bottomRight & right b %~ addWidth s (-0.15))
                    10
      ]

  Tree b _ (r, g, b') ->
    [ Fill 139 69 19 $ RoundedRect (s^.topLeft & left %~ addWidth s 0.333)
                          (s^.bottomRight & right b %~ addWidth s (-0.3333))
                          0
    , Fill r g b' $ RoundedRect (s^.topLeft)
                                (s^.bottomRight & bottom %~ addHeight s (-0.15))
                                10
    ]

  Hill b p (r, g, b') ->
    [ Fill r g b' $ BezierCurve (s^.bottomLeft)
                               (s^.topLeft & left .~ fst (centerPoint b p))
                               (s^.bottomRight)
    ]

collides :: Sprite -> Sprite -> Bool
collides sa sb =
  sa^.bottomRight > sb^.topLeft && sb^.bottomRight < sa^.topLeft

contains :: (Position, Position) -> Sprite -> Bool
contains (topLeft', bottomRight') s =
  s^.bottomRight > topLeft' && bottomRight' > s^.topLeft

applyGravity :: Int -> Int -> Sprite -> Sprite
applyGravity constant lowerBound s =
  case s of
    User _ _ _
      | lowerBound <= s^.position.bottom -> s & velocity %~ ground
      | otherwise                        -> s & velocity %~ gravityVeloc constant
    x -> x

gravityVeloc :: Int -> Velocity -> Velocity
gravityVeloc constant v = v & yv %~ min 20 . (+ constant)

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
ground = yv %~ min 0

wall :: Velocity -> Velocity
wall = xv .~ 0

advanceSprite :: Velocity -> Sprite -> Sprite
advanceSprite characterV s = case s of
  User _ _ _  -> s & position %~ applyVeloc (wall characterV)
  _           -> s & position %~ applyVeloc (characterV & yv .~ 0 & xv %~ negate)
                               . maybe id applyVeloc (s ^? velocity)

applyVeloc :: Velocity -> Position -> Position
applyVeloc (Velocity vx vy) (Position x y) = Position (x + vx) (y + vy)

operationVelocity :: Op -> Velocity -> Velocity
operationVelocity op v =
  case op of
    UpOp -> v & yv %~ max (-12) . subtract 2
    DownOp -> v & yv %~ min 12 . (+ 4)
    RightOp -> v & xv %~ min 12 . (+ 2)
    LeftOp ->  v & xv %~ max (-12) . subtract 2
    NoOp -> v

nextTick :: [Op] -> Scene -> Scene
nextTick op scene@Scene{..} =
  scene { foreground = fmap (next []) $ foreground
        , character
            = bound
            . applyFriction friction
            . applyGravity gravity screenHeight
            . next stage
            $ character & velocity %~ \xs -> foldr operationVelocity xs op
        , stage = fmap (next [character]) stage
        , background = fmap (next []) background
        }
  where
  Just charV = character^?velocity

  next :: [Sprite] -> Sprite -> Sprite
  next xs x =
    if any (==True) (fmap (collides x) xs) then x
    else advanceSprite charV x

  bound s = s & position %~ over bottom (max 0)
                          . over bottom (min screenHeight)

drawScene :: Scene -> [Shape]
drawScene Scene{..} =
  [Fill 149 207 174 $ Rect (Position 0 0) (Position screenWidth screenHeight)]
    <> draw' background
    <> draw' stage
    <> draw character
    <> draw' foreground
  where
  draw' = concatMap draw
        . filter inView

inView :: Sprite -> Bool
inView = contains (Position 0 0, Position 800 800)
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
