module Main where

import Test.QuickCheck;

-- Exercise 1
data GTree a = Leaf a | Gnode [GTree a]
    deriving (Eq, Show)

-- 1.1
depth :: GTree a -> Integer
depth (Leaf _) = 1
depth (Gnode []) = 1
depth (Gnode xs) = 1 + maximum (map depth xs)

prop_depth_0 = 
    depth (Leaf 5) == 1

prop_depth_1 = 
    depth (Gnode []) == 1

prop_depth_2 = 
    depth (Gnode [Leaf 1, Leaf 2, Leaf 3]) == 2

prop_depth_3 = 
    depth (Gnode [Leaf 1, Gnode [Leaf 10, Gnode [Leaf 20]]]) == 4

-- 1.2
contains :: Eq a => GTree a -> a -> Bool
contains (Leaf x) y = x == y
contains (Gnode []) _ = False
contains (Gnode xs) y = foldr (||) False (map (\t -> contains t y) xs)

prop_contains_0 = 
    contains (Gnode [Leaf 1, Leaf 2, Leaf 3]) 2 == True

prop_contains_1 = 
    contains (Gnode [Leaf 1, Leaf 2, Leaf 3]) 99 == False

prop_contains_2 = 
    contains (Gnode [Leaf 1, Gnode [Leaf 10, Gnode [Leaf 20]]]) 20 == True

prop_contains_3 = 
    contains (Leaf "hi") "hi" == True

-- 1.3
mapTree :: (a -> b) -> GTree a -> GTree b
mapTree (func) (Leaf x) = Leaf (func x)
mapTree (func) (Gnode xs) = Gnode (map (mapTree func) xs)

prop_mapTree_0 = 
    mapTree (+1) (Gnode [Leaf 1, Leaf 2, Leaf 3]) == Gnode [Leaf 2, Leaf 3, Leaf 4]

prop_mapTree_1 = 
    mapTree (> 5) (Gnode [Leaf 1, Gnode [Leaf 10, Gnode [Leaf 20]]]) == Gnode [Leaf False, Gnode [Leaf True, Gnode [Leaf True]]]

prop_mapTree_2 = 
    mapTree (*2) (Leaf 10) == Leaf 20

prop_mapTree_3 = 
    mapTree id (Gnode [] :: GTree Integer) == Gnode []

-- Exercise 2

-- Exercise 3

-- Exercise 4

-- Exercise 5

-- Exercise 6


main = do
    quickCheck prop_depth_0
    quickCheck prop_depth_1
    quickCheck prop_depth_2
    quickCheck prop_depth_3

    quickCheck prop_contains_0
    quickCheck prop_contains_1
    quickCheck prop_contains_2
    quickCheck prop_contains_3

    quickCheck prop_mapTree_0
    quickCheck prop_mapTree_1
    quickCheck prop_mapTree_2
    quickCheck prop_mapTree_3
