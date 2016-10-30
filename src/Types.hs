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

data Velocity = Velocity !Int !Int deriving (Eq, Show)
instance Monoid Velocity where
  mempty = Velocity 0 0
  Velocity x y `mappend` Velocity xx yy = Velocity (x + xx) (y + yy)

data Position = Position !Int !Int deriving (Eq, Ord, Show)

type RGB = (Int, Int, Int)

colors :: [RGB]
colors =
  [ (243, 211, 12)
  , (88, 134, 64)
  , (225, 81, 44)
  , (184, 104, 33)
  ]

randomColor :: StdGen -> (RGB, StdGen)
randomColor g =
  let (i, newGen) = randomR (0, length colors - 1) g
  in (colors !! i, newGen)

data Sprite
  = User !Bounds !Position !Velocity
  | Leaf !Bounds !Position RGB !Velocity
  | Cloud !Bounds !Position !Velocity
  | Tree !Bounds !Position RGB
  | Hill !Bounds !Position RGB
  deriving (Eq, Show)

randomCloud :: StdGen -> (Sprite, StdGen)
randomCloud g =
  let (x, g') = randomR (0, 3600) g
      (y, g'') = randomR (40, 400) g'
      (w, g''') = randomR (100, 200) g''
      (h, g'''') = randomR (20, 40) g'''
      (vx, newGen) = randomR (-1, -2) g''''
      b = Bounds w h
      p = Position x y
      v = Velocity vx 0
   in (Cloud b p v, newGen)

randomHill :: StdGen -> (Sprite, StdGen)
randomHill g =
  let (x, g') = randomR (0, 3600) g
      (w, g'') = randomR (300, 500) g'
      (c, g''') = randomColor g''
      (h, newGen) = randomR (70, 100) g'''
      b = Bounds w h
      p = Position x 800
   in (Hill b p c, newGen)

randomTree :: StdGen -> (Sprite, StdGen)
randomTree g =
  let (x, g') = randomR (0, 3600) g
      (w, g'') = randomR (30, 40) g'
      (h, g''') = randomR (100, 120) g''
      (c, newGen) = randomColor g'''
      b = Bounds w h
      p = Position x 800
   in (Tree b p c, newGen)

defaultUser :: Sprite
defaultUser = User (Bounds 24 30) (Position 100 100) mempty

randomLeaf :: StdGen -> (Sprite, StdGen)
randomLeaf g =
  let (x, g') = randomR (0, 3600) g
      (y, g'') = randomR (600, 790) g'
      (c, g''') = randomColor g''
      (vx, newGen) = randomR (-1, -2) g'''
      b = Bounds 5 5
      p = Position x y
      v = Velocity vx 0
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

yv :: Lens' Velocity Int
yv = lens (\(Velocity _ y) -> y) (\(Velocity x _) y -> Velocity x y)

xv :: Lens' Velocity Int
xv = lens (\(Velocity x _) -> x) (\(Velocity _ y) x -> Velocity x y)

xp :: Lens' Position Float
xp = lens (\(Position x _) -> fromIntegral x) (\(Position _ y) x -> Position (floor x) y)

yp :: Lens' Position Float
yp = lens (\(Position _ y) -> fromIntegral y) (\(Position x _) y -> Position x (floor y))

left :: Lens' Position Int
left = lens (\(Position l _) -> l) (\(Position _ b) l -> Position l b)

right :: Bounds -> Lens' Position Int
right (Bounds w _) = lens get' set'
  where
  get' p = p^.left + w
  set' p s = p & left .~ (s - w)

bottom :: Lens' Position Int
bottom = lens get' set'
  where
  get' (Position _ b) = b
  set' (Position l _) b = Position l b

top :: Bounds -> Lens' Position Int
top (Bounds _ h) = lens get' set'
  where
  get' p = p^.bottom - h
  set' p s = p & bottom .~ (s + h)

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

centerPoint :: Bounds -> Position -> (Int, Int)
centerPoint b@(Bounds _ h) (Position x y) = (x + halfWidth b, y + (h *~ 0.5))

data Shape
  = Rect !Position !Position
  | RoundedRect !Position !Position !Radius
  | BezierCurve !Position !Position !Position
  | Fill !Int !Int !Int !Shape
  deriving (Show, Eq)

