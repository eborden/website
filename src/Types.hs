{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Types where

import Lens.Micro

(*~) :: Int -> Float -> Int
x *~ y = round $ fromIntegral x * y

type Point = (Int, Int)
type Radius = Int
data Bounds = Bounds {width :: Int, height :: Int} deriving (Eq, Show)
data Velocity = Velocity Int Int deriving (Eq, Show)
instance Monoid Velocity where
  mempty = Velocity 0 0
  Velocity x y `mappend` Velocity xx yy = Velocity (x + xx) (y + yy)

data Position = Position Int Int deriving (Eq, Ord, Show)

data Sprite
  = User Bounds Position Velocity
  | Leaf Bounds Position Velocity
  | Cloud Bounds Position Velocity
  | Tree Bounds Position
  | Hill Bounds Position
  deriving (Eq, Show)

defaultUser :: Sprite
defaultUser = User (Bounds 20 20) (Position 100 100) mempty

bounds :: Lens' Sprite Bounds
bounds = lens get' set'
  where
  get' = \case
    User b _ _  -> b
    Leaf b _ _  -> b
    Cloud b _ _ -> b
    Tree b _    -> b
    Hill b _    -> b
  set' x b = case x of
    User _ p v  -> User b p v
    Leaf _ p v  -> Leaf b p v
    Cloud _ p v -> Cloud b p v
    Tree _ p    -> Tree b p
    Hill _ p    -> Hill b p 

position :: Lens' Sprite Position
position = lens get' set'
  where
  get' = \case
    User _ p _  -> p
    Leaf _ p _  -> p
    Cloud _ p _ -> p
    Tree _ p    -> p
    Hill _ p    -> p
  set' x p = case x of
    User b _ v  -> User b p v
    Leaf b _ v  -> Leaf b p v
    Cloud b _ v -> Cloud b p v
    Tree b _    -> Tree b p
    Hill b _    -> Hill b p 

velocity :: Traversal' Sprite Velocity
velocity f s = update s
  where
  update = \case
    User b p v  -> User b p <$> f v
    Leaf b p v  -> Leaf b p <$> f v
    Cloud b p v -> Cloud b p <$> f v
    Tree b p    -> pure $ Tree b p
    Hill b p    -> pure $ Hill b p 

xv :: Lens' Velocity Int
xv = lens (\(Velocity x _) -> x) (\(Velocity _ x) y -> Velocity x y)

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
  set' p s = p & left .~ (s + w)

bottom :: Lens' Position Int
bottom = lens get' set'
  where
  get' (Position _ b) = b
  set' (Position l _) b = Position l b

top :: Bounds -> Lens' Position Int
top (Bounds _ h) = lens get' set'
  where
  get' p = p^.bottom - h
  set' p s = p & bottom .~ (s - h)

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
  = Rect Position Position
  | RoundedRect Position Position Radius
  | BezierCurve Position Position Position
  | Fill Int Int Int Shape
  deriving (Show, Eq)

