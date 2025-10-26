module Main where

import Test.QuickCheck;

-- Exercise 1
sumList :: [Float] -> Float
sumList [] = 0
sumList (x:xs) = x + sumList xs 

lengthList :: [Float] -> Float
lengthList [] = 0
lengthList (x:xs) = 1 + lengthList xs

average :: [Float] -> Float
average [] = 0
average l = (sumList l) / (lengthList l)

prop_average_0 =
    average [0, 1, 2, 3, 4, 5] == 2.5

prop_average_1 =
    average [] == 0

prop_average_2 =
    average [-1, 0, 1] == 0

prop_average_3 =
    average [3.3, 3.3, 3.3] == 3.3

main = do
    -- Exercise 1 tests
    quickCheck prop_average_0
    quickCheck prop_average_1
    quickCheck prop_average_2
    quickCheck prop_average_3
