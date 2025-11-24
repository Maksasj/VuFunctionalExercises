module Main where

import Test.QuickCheck;

-- Exercise 1
data Coordinates = Coordinates Float Float
    deriving (Show, Ord, Eq)

data Shape = Circle Float Coordinates | Rectangle Float Float Coordinates
    deriving (Show, Ord, Eq)

area :: Shape -> Float
area (Circle r _) = pi*r*r
area (Rectangle h w _) = h*w

isRound :: Shape -> Bool
isRound (Circle _ (Coordinates x y)) = True
isRound (Rectangle _ _ (Coordinates x y)) = False

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

distance :: Coordinates -> Coordinates -> Float
distance (Coordinates x0 y0) (Coordinates x1 y1) = sqrt((x0 - x1)*(x0 - x1) + (y0 - y1)*(y0 - y1)) 

aabbCollission :: Coordinates -> Float -> Float -> Coordinates -> Float -> Float -> Bool
aabbCollission (Coordinates x0 y0) w0 h0 (Coordinates x1 y1) w1 h1 = (x0 < x1 + w1 && x0 + w0 > x1) && (y0 < y1 + h1 && y0 + h0 > y1);

rectCircleCollission :: Coordinates -> Float -> Float -> Coordinates -> Float ->  Bool
rectCircleCollission (Coordinates xr yr) w h (Coordinates xc yc) r  =
    let 
        px = clamp xc xr (xr + w)
        py = clamp yc yr (yr + h)

        dx = xc - px
        dy = yc - py

        distanceSq = dx * dx + dy * dy
        radiusSq = r * r
    in
        distanceSq < radiusSq

overlaps :: Shape -> Shape -> Bool
overlaps (Circle r0 c0) (Circle r1 c1) = (distance c0 c1) < r0 + r1
overlaps (Rectangle w0 h0 c0) (Rectangle w1 h1 c1) = aabbCollission c0 w0 h0 c1 w1 h1
overlaps (Rectangle w1 h1 c1) (Circle r0 c0) = rectCircleCollission c1 w1 h1 c0 r0
overlaps (Circle r0 c0) (Rectangle w1 h1 c1) = rectCircleCollission c1 w1 h1 c0 r0

prop_isRound_Circle_0 r x y =
    (isRound (Circle r (Coordinates x y))) == True

prop_isRound_Rectangle_0 w h x y =
    (isRound (Rectangle w h (Coordinates x y))) == False

main = do
    -- Exercise 1 tests
    quickCheck prop_isRound_Circle_0 
    quickCheck prop_isRound_Rectangle_0
