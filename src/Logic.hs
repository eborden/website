{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Logic
  (module Logic, defaultUser
  ) where

import Data.Monoid
import Lens.Micro
import Types
import System.Random

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
  , gravity      :: Float
  , friction     :: Float
  } deriving (Show, Eq)

addHeight :: Sprite -> Float -> Float -> Float
addHeight s m y = (fromIntegral (s^.bounds&height) * m) + y

addWidth :: Sprite -> Float -> Float -> Float
addWidth s m x = (fromIntegral (s^.bounds&width) * m) + x

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
          (s^.topRight & top b %~ addHeight s 0.4)
          3
      , Fill 219 183 132
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.1
                      & left %~ addWidth s 0.1)
          (s^.topRight & top b %~ addHeight s 0.4
                       & right b %~ addWidth s (-0.1))
          3
      , Fill 102 88 73
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.4
                      & left %~ addWidth s 0.2)
          (s^.bottomRight & right b %~ addWidth s (-0.2))
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
          (s^.topRight & top b %~ addHeight s 0.4)
          3
      , Fill 219 183 132
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.1
                      & left %~ addWidth s 0.3)
          (s^.topRight & top b %~ addHeight s 0.4)
          3
      , Fill 102 88 73
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.4
                      & left %~ addWidth s 0.2)
          (s^.bottomRight & right b %~ addWidth s (-0.2))
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
          (s^.topRight & top b %~ addHeight s 0.4)
          3
      , Fill 219 183 132
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.1)
          (s^.topRight & top b %~ addHeight s 0.4
                       & right b %~ addWidth s (-0.3))
          3
      , Fill 102 88 73
      $ RoundedRect
          (s^.topLeft & top b %~ addHeight s 0.4
                      & left %~ addWidth s 0.2)
          (s^.bottomRight & right b %~ addWidth s (-0.2))
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
  sb^.bottomRight > sa^.topLeft
  && sa^.bottomRight > sb^.topLeft
  && sa^.position.bottom > sb^.topLeft.yp

contains :: (Position, Position) -> Sprite -> Bool
contains (topLeft', bottomRight') s =
  s^.bottomRight > topLeft' && bottomRight' > s^.topLeft

applyGravity :: Float -> Int -> Sprite -> Sprite
applyGravity constant lowerBound s =
  case s of
    User _ _ _
      | fromIntegral lowerBound <= s^.position.bottom
          -> s & velocity %~ ground
      | otherwise -> s & velocity %~ gravityVeloc constant
    x -> x

gravityVeloc :: Float -> Velocity -> Velocity
gravityVeloc constant v = v & yv %~ min 20 . (+ constant)

applyFriction :: Float -> Sprite -> Sprite
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

leafJitter :: Float -> Sprite -> Sprite
leafJitter lower s = case s of
  Leaf b p c (Velocity x y)
    | s^.position.yp <= (lower - 200) -> Leaf b p c . Velocity x $ negate y
    | s^.position.yp >= lower -> Leaf b p c . Velocity x $ negate y
    | otherwise -> s
  _ -> s

randomNegate :: Num i => StdGen -> (i -> i, StdGen)
randomNegate g =
  let (x, newG) = randomR (False, True) g
  in (if x then id else negate, newG)

nextTick :: [Op] -> Scene -> Scene
nextTick op scene@Scene{..} =
  scene { foreground = fmap (tick . leafJitter (fromIntegral screenHeight)) $ foreground
        , character
            = bound
            . applyFriction friction
            . applyGravity gravity screenHeight
            . tick
            $ if collision then character else char
        , stage = fmap tick stage
        , background = fmap tick background
        }
  where
  Just charV = character^?velocity

  collision = any (collides character) $ stage
  char = character & velocity %~ \xs -> foldr operationVelocity xs op

  tick :: Sprite -> Sprite
  tick x =
    wrapAround charV (0, fromIntegral screenWidth * 3)
      $ if collision
          then advanceSprite mempty x
          else advanceSprite charV x

  bound s = s & position %~ over bottom (max 0)
                          . over bottom (min (fromIntegral screenHeight))

drawScene :: Scene -> [Shape]
drawScene scene@Scene{..} =
  [Fill 149 207 174 $ Rect (Position 0 0) (Position (fromIntegral screenWidth) (fromIntegral screenHeight))]
    <> draw' background
    <> draw' stage
    <> draw character
    <> draw' foreground
  where
  draw' = concatMap draw
        . filter (inView scene)

inView :: Scene -> Sprite -> Bool
inView Scene{..} = contains (Position 0 0, Position (fromIntegral screenWidth) (fromIntegral screenHeight))

wrapAround :: Velocity -> (Float, Float) -> Sprite -> Sprite
wrapAround v (leftBound, rightBound) s
  | velocLeft && beyondLeft = s & position.left .~ rightBound
  | velocRight && beyondRight = s & position.right bound .~ leftBound
  | otherwise = s
  where
    bound = s^.bounds
    beyondLeft = s^.position.right bound <= leftBound
    beyondRight = s^.position.left >= rightBound
    velocLeft = v^.xv >= 0
    velocRight = v^.xv < 0
