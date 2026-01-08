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

-- Exercise 2
any0 :: (a->Bool) -> [a] -> Bool
any0 _ [] = False
any0 predicate (x:xs) = predicate x || any0 predicate xs

all0 :: (a->Bool) -> [a] -> Bool
all0 _ [] = True
all0  predicate (x:xs) = predicate x && all0 predicate xs

any1 :: Eq a => (a->Bool) -> [a] -> Bool
any1 predicate l = (filter predicate l) /= []

all1 :: Eq a => (a->Bool) -> [a] -> Bool
all1 predicate l = (filter predicate l) == l

any2 :: (a->Bool) -> [a] -> Bool
any2 predicate l = foldr (||) False (map predicate l)

all2 :: (a->Bool) -> [a] -> Bool
all2 predicate l = foldr (\y x -> predicate x && y) True l

-- Exercise 3
unzip :: [(a, b)] -> ([a], [b])
unzip l = foldr predicate ([], []) l 
    where 
        predicate :: (a, b) -> ([a], [b]) -> ([a], [b])
        predicate (x, y) (l0, l1) = (x:l0, y:l1)

-- Exercise 4
length0 :: Eq a => [a] -> Int
length0 [] = 0
length0 (x:xs) = 1 + length0 xs

length1 :: [a] -> Int
length1 l = (sum . (map (\_ -> 1))) l

length2 :: [a] -> Int
length2 l = foldr (\_ ->(\x -> x + 1)) 0 l  

-- Exercise 5
sumUpToBound :: Integer -> [Integer] -> Integer
sumUpToBound mv l = foldl predicate 0 l
    where
        predicate :: Integer -> Integer -> Integer
        predicate b0 v
            | (b0 + v) <= mv = b0 + v
            | otherwise = b0

ff :: Integer -> [Integer] -> Integer
ff v l = ((sumUpToBound v) . (map (* 10)) . (filter (>= 0))) l

-- Exercise 6
total :: (Integer -> Integer) -> Integer -> Integer
total predicate n = foldl pr 0 [0..n]
    where
        pr :: Integer -> Integer -> Integer
        pr a b = b + (predicate a)

-- Exercise 7
-- f ::a -> a
iter0 :: Int -> (a -> a) -> (a -> a)
iter0 n f
  | n <= 0 = id
  | otherwise = f . iter0 (n-1) f

iter1 :: Int -> (a -> a) -> (a -> a)
iter1 n f
  | n <= 0 = id
  | otherwise = foldr (.) id (replicate (n) f)

-- Exercise 8
splits :: [a] -> [([a],[a])]
splits xs = [ (take i xs, drop i xs) | i <- [0 .. length xs] ]

main = do
    -- Exercise 1 tests
    quickCheck prop_isRound_Circle_0 
    quickCheck prop_isRound_Rectangle_0
