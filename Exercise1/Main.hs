module Main where

import Test.QuickCheck;

-- Exercise 1
nAnd0 :: Bool -> Bool -> Bool
nAnd0 x y = not (x && y)

nAnd1 :: Bool -> Bool -> Bool
nAnd1 True True = False
nAnd1 _ _ = True

nAnd2 :: Bool -> Bool -> Bool
nAnd2 False False = True
nAnd2 False True = True
nAnd2 True False = True
nAnd2 True True = False

prop_nAnd0_1 x y =
    nAnd0 x y == nAnd1 x y

prop_nAnd1_1 x y =
    nAnd1 x y == nAnd2 x y

prop_nAnd2_1 x y =
    nAnd2 x y == nAnd0 x y

-- Exercise 2
nDigits :: Integer -> Int
nDigits n
    | n >= 0 = length(show n)
    | otherwise = length(show n) - 1

prop_nDigits_0 =
    nDigits 1 == 1 

prop_nDigits_1 =
    nDigits 100 == 3 

prop_nDigits_2 =
    nDigits 12859158192521515298591 == 23

prop_nDigits_3 =
    nDigits 0 == 1 

prop_nDigits_4 =
    nDigits (-1) == 1 

prop_nDigits_5 =
    nDigits (-24910) == 5 

prop_nDigits_6 =
    nDigits (-0) == 1 

-- Exercise 3
nRoots :: Float -> Float -> Float -> Int
nRoots a b c
    | a == 0 = error "the first argument should be non-zero!"
    | b*b > 4.0*a*c = 2
    | b*b < 4.0*a*c = 0
    | otherwise = 1

prop_nRoots_2_0 = nRoots (-2) (-9) (5) == 2 -- x1 ~= -5.0, x2 ~= 0.5
prop_nRoots_2_1 = nRoots (1) (-6) (5) == 2 -- x1 ~= 5.0, x2 ~= 1.0
prop_nRoots_2_2 = nRoots (5) (-10) (-8) == 2 -- x1 ~= 2.61, x2 ~= 0.61

prop_nRoots_1_0 = nRoots (3) (-30) (75) == 1 -- x ~= 5 
prop_nRoots_1_1 = nRoots (4) (-8) (4) == 1 -- x1 ~= 1 
prop_nRoots_1_2 = nRoots (2) (-8) (8) == 1 -- x1 ~= 2 

prop_nRoots_0_0 = nRoots (-1) (1) (-2) == 0 -- no roots 
prop_nRoots_0_1 = nRoots (3) (1) (5) == 0 -- no roots
prop_nRoots_0_2 = nRoots (-4) (-4) (-5) == 0 -- no roots

prop_nRoots_error = nRoots (0) (1) (1) == 0 -- expect error

-- Exercise 4
quadraticRoot :: (Float -> Float -> Float) -> Float -> Float -> Float -> Float
quadraticRoot op a b c = ((op) (-b) (sqrt (b*b - 4 * a * c))) / (2 * a)

plusRoot :: Float -> Float -> Float -> Float
plusRoot a b c = quadraticRoot (+) a b c

minusRoot :: Float -> Float -> Float -> Float
minusRoot a b c = quadraticRoot (-) a b c

-- quadraticRoots :: Float -> Float -> Float -> (Float, Float)
-- quadraticRoots a b c = ((plusRoot) a b c, (minusRoot) a b c)

chooseRoot :: (Float -> Float -> Float) -> Float -> Float -> Float -> Float 
chooseRoot predicate a b c 
    | nRoots a b c == 2 = (predicate) (plusRoot a b c) (minusRoot a b c)
    | nRoots a b c == 1 = plusRoot a b c
    | otherwise = error "the quadratic equation have no roots!"

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c = chooseRoot min a b c 

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c = chooseRoot max a b c 

-- Exercise 5
power2 :: Integer -> Integer 
power2 a
    | a == 0 = 1
    | a > 0 = 2 * power2 (a - 1)
    | a < 0 = 0

prop_power2_0 = power2 (-5) == 0

prop_power2_1 a
    | a < 0 = True
    | otherwise = power2 a == 2^a

-- Exercise 6
mult :: Integer -> Integer -> Integer
mult a b
    | (b == 0) || (a == 0) = 0
    | b < 0 = (-a) + (mult a (b + 1))
    | otherwise = a + (mult a (b - 1))
    -- | (a < 0) && (b > 0) = a + (mult a (b - 1))
    -- | (a > 0) && (b < 0) = (-a) + (mult a (b + 1))
    -- | (a < 0) && (b < 0) = (-a) + (mult a (b + 1))

prop_mult_0 = 
    (mult 0 0) == 0 &&
    (mult 0 1) == 0 &&
    (mult 1 0) == 0 &&
    (mult 1 1) == 1 &&
    (mult (-1) 1) == (-1) * 1 &&
    (mult 1 (-1)) == (-1) &&
    (mult (-1) (-1)) == 1 

prop_mult_1 a b = 
    mult a b == a * b

-- Exercise 7
prod :: Integer -> Integer -> Integer
prod m n 
    | m > n = error "range is invalid!"     
    | m == n = m
    | otherwise = m * (prod (m + 1) n)

prop_prod_0 =
    prod 0 5 == 0 * 1 * 2 * 3 * 4 * 5

prop_prod_1 =
    prod 13 17 == 13 * 14 * 15 * 16 * 17

prop_prod_2 =
    prod (-2) 3 == 0

prop_prod_3 =
    prod (-5) (-3) == (-5) * (-4) * (-3)

prop_prod_4 =
    prod 1 1 == 1

fact :: Integer -> Integer
fact x = prod 1 x

prop_fact_0 x 
    | x <= 0 = True
    | otherwise = fact x == product [1..x]

main = do
    -- Exercise 1 tests
    quickCheck prop_nAnd0_1
    quickCheck prop_nAnd1_1
    quickCheck prop_nAnd2_1

    -- Exercise 2 tests
    quickCheck prop_nDigits_0
    quickCheck prop_nDigits_1
    quickCheck prop_nDigits_2
    quickCheck prop_nDigits_3
    quickCheck prop_nDigits_4
    quickCheck prop_nDigits_5
    quickCheck prop_nDigits_6

    -- Exercise 3 tests
    quickCheck prop_nRoots_2_0
    quickCheck prop_nRoots_2_1
    quickCheck prop_nRoots_2_2

    quickCheck prop_nRoots_1_0
    quickCheck prop_nRoots_1_1
    quickCheck prop_nRoots_1_2
    
    quickCheck prop_nRoots_0_0
    quickCheck prop_nRoots_0_1
    quickCheck prop_nRoots_0_2

    -- quickCheck prop_nRoots_error 

    -- Exercise 5 tests
    quickCheck prop_power2_0
    quickCheck prop_power2_1

    -- Exercise 6 tests
    quickCheck prop_mult_0
    quickCheck prop_mult_1

    -- Exercise 7 tests
    quickCheck prop_prod_0
    quickCheck prop_prod_1
    quickCheck prop_prod_2
    quickCheck prop_prod_3
    quickCheck prop_prod_4

    quickCheck prop_fact_0
