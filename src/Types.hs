{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Types where

import Lens.Micro
import System.Random

(*~) :: Int -> Float -> Int
x *~ y = round $ fromIntegral x * y

type Point = (Int, Int)
type Radius = Int

data Bounds = Bounds {width :: !Int, height :: !Int} deriving (Eq, Show)

data Velocity = Velocity !Float !Float deriving (Eq, Show)
instance Monoid Velocity where
  mempty = Velocity 0 0
  Velocity x y `mappend` Velocity xx yy = Velocity (x + xx) (y + yy)

data Position = Position !Float !Float deriving (Eq, Ord, Show)

data RGB = RGB !Int !Int !Int deriving (Eq, Ord, Show)

colors :: [RGB]
colors =
  [ RGB 243 211 12
  , RGB 88 134 64
  , RGB 225 81 44
  , RGB 184 104 33
  ]

randomColor :: StdGen -> (RGB, StdGen)
randomColor g =
  let (i, newGen) = randomR (0, length colors - 1) g
  in (colors !! i, newGen)

data Sprite
  = User !Bounds !Position !Velocity
  | Leaf !Bounds !Position !RGB !Velocity
  | Cloud !Bounds !Position !Velocity
  | Tree !Bounds !Position !RGB
  | Hill !Bounds !Position !RGB
  deriving (Eq, Show)

panelWidth :: Num a => Bounds -> a
panelWidth b = fromIntegral $ width b * 3

width_, height_ :: Num a => Bounds -> a
width_ = fromIntegral . width
height_ = fromIntegral . height

randomCloud :: Bounds -> StdGen -> (Sprite, StdGen)
randomCloud screen g =
  let (x, g') = randomR (0, panelWidth screen) g
      (y, g'') = randomR (height_ screen / 20, height_ screen / 2) g'
      (w, g''') = randomR (width screen `div` 8, width screen `div` 4) g''
      (h, g'''') = randomR (height screen `div` 80, height screen `div` 20) g'''
      (vx, newGen) = randomR (-0.1, -0.3) g''''
      b = Bounds w h
      p = Position x y
      v = Velocity vx 0
   in (Cloud b p v, newGen)

randomHill :: Bounds -> StdGen -> (Sprite, StdGen)
randomHill screen g =
  let (x, g') = randomR (0, panelWidth screen) g
      (w, g'') = randomR ((width screen `div` 10) * 3, (width screen `div` 10) * 5) g'
      (c, g''') = randomColor g''
      (h, newGen) = randomR ((height screen `div` 80) * 2, (height screen `div` 80) * 12) g'''
      b = Bounds w h
      p = Position x . fromIntegral $ height screen
   in (Hill b p c, newGen)

randomTree :: Bounds -> StdGen -> (Sprite, StdGen)
randomTree screen g =
  let (x, g') = randomR (0, panelWidth screen) g
      (w, g'') = randomR (30, 50) g'
      (h, g''') = randomR (100, 120) g''
      (c, newGen) = randomColor g'''
      b = Bounds w h
      p = Position x . fromIntegral $ height screen
   in (Tree b p c, newGen)

defaultUser :: Bounds -> Sprite
defaultUser screen = User (Bounds 24 30) (Position (width_ screen / 2) 0) mempty

randomLeaf :: Bounds -> StdGen -> (Sprite, StdGen)
randomLeaf screen g =
  let (x, g') = randomR (0, panelWidth screen) g
      (y, g'') = randomR ((height_ screen / 8) * 6, height_ screen) g'
      (c, g''') = randomColor g''
      (vx, g'''') = randomR (-0.4, -1) g'''
      (vy, newGen) = randomR (-0.2, -0.3) g''''
      b = Bounds 5 5
      p = Position x y
      v = Velocity vx vy
   in (Leaf b p c v, newGen)

bounds :: Lens' Sprite Bounds
bounds = lens get' set'
  where
  get' = \case
    User b _ _   -> b
    Leaf b _ _ _ -> b
    Cloud b _ _  -> b
    Tree b _ _   -> b
    Hill b _ _   -> b
  set' x b = case x of
    User _ p v   -> User b p v
    Leaf _ p c v -> Leaf b p c v
    Cloud _ p v  -> Cloud b p v
    Tree _ p c   -> Tree b p c
    Hill _ p c   -> Hill b p c

position :: Lens' Sprite Position
position = lens get' set'
  where
  get' = \case
    User _ p _   -> p
    Leaf _ p _ _ -> p
    Cloud _ p _  -> p
    Tree _ p _   -> p
    Hill _ p _   -> p
  set' x p = case x of
    User b _ v   -> User b p v
    Leaf b _ c v -> Leaf b p c v
    Cloud b _ v  -> Cloud b p v
    Tree b _ c   -> Tree b p c
    Hill b _ c   -> Hill b p c

velocity :: Traversal' Sprite Velocity
velocity f s = update s
  where
  update = \case
    User b p v   -> User b p <$> f v
    Leaf b p c v -> Leaf b p c <$> f v
    Cloud b p v  -> Cloud b p <$> f v
    Tree b p c   -> pure $ Tree b p c
    Hill b p c   -> pure $ Hill b p c

yv :: Lens' Velocity Float
yv = lens (\(Velocity _ y) -> y) (\(Velocity x _) y -> Velocity x y)

xv :: Lens' Velocity Float
xv = lens (\(Velocity x _) -> x) (\(Velocity _ y) x -> Velocity x y)

-- Rounded for canvas efficiency
xp, yp :: Lens' Position Float
xp = lens (\(Position x _) -> fromIntegral $ (round x :: Int))
          (\(Position _ y) x -> Position x y)
yp = lens (\(Position _ y) -> fromIntegral $ (round y :: Int))
          (\(Position x _) y -> Position x y)

left :: Lens' Position Float
left = lens (\(Position l _) -> l) (\(Position _ b) l -> Position l b)

right :: Bounds -> Lens' Position Float
right (Bounds w _) = lens get' set'
  where
  get' p = p^.left + fromIntegral w
  set' p s = p & left .~ (s - fromIntegral w)

bottom :: Lens' Position Float
bottom = lens get' set'
  where
  get' (Position _ b) = b
  set' (Position l _) b = Position l b

top :: Bounds -> Lens' Position Float
top (Bounds _ h) = lens get' set'
  where
  get' p = p^.bottom - fromIntegral h
  set' p s = p & bottom .~ (s + fromIntegral h)

topLeft :: Lens' Sprite Position
topLeft = lens get' set'
  where
  get' s = Position (s^.position . left) (s^.position.top (s^.bounds))
  set' s p = s & position %~ set left (p^.left)
                           . set (top (s^.bounds)) (p^.bottom)

topRight :: Lens' Sprite Position
topRight = lens get' set'
  where
  get' s = Position (s^.position . right (s^.bounds)) (s^.position.top (s^.bounds))
  set' s p = s & position %~ set (right (s^.bounds)) (p^.left)
                           . set (top (s^.bounds)) (p^.bottom)

bottomLeft :: Lens' Sprite Position -- Position -> (Int, Int)
bottomLeft = lens get' set'
  where
  get' s = s^.position
  set' s p = s & position .~ p

bottomRight :: Lens' Sprite Position
bottomRight = lens get' set'
  where
  get' s = Position (s^.position . right (s^.bounds)) (s^.position.bottom)
  set' s p = s & position %~ set (right (s^.bounds)) (p^.left)
                           . set bottom (p^.bottom)

halfWidth :: Bounds -> Int
halfWidth (Bounds w _) = w *~ 0.5

centerPoint :: Bounds -> Position -> (Float, Float)
centerPoint b@(Bounds _ h) (Position x y) =
  (x + fromIntegral (halfWidth b), y + (fromIntegral h * 0.5))

data Shape
  = Rect !Position !Position
  | RoundedRect !Position !Position !Radius
  | BezierCurve !Position !Position !Position
  | Fill !Int !Int !Int !Shape
  deriving (Show, Eq)

