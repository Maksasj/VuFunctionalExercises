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

-- Exercise 2
divides0Loop :: Integer -> Integer -> [Integer]
divides0Loop x i
    | i > x = []
    | ((x `mod` i) == 0) = i:(divides0Loop x (i + 1)) 
    | otherwise = divides0Loop x (i + 1)

divides0 :: Integer -> [Integer]
divides0 x 
    | x <= 0 = error "Number could not be less or equal to 0"    
    | otherwise = divides0Loop x 1

prop_divides0_0 =
    divides0 24 == [1, 2, 3, 4, 6, 8, 12, 24]

prop_divides0_1 =
    divides0 1 == [1]

prop_divides0_2 =
    divides0 15 == [1, 3, 5, 15]

divides1 :: Integer -> [Integer]
divides1 x 
    | x <= 0 = error "Number could not be less or equal to 0"    
    | otherwise = [n | n <- [1..x], (x `mod` n == 0)] 

prop_divides1_0 =
    divides0 24 == [1, 2, 3, 4, 6, 8, 12, 24]

prop_divides1_1 =
    divides1 1 == [1]

prop_divides1_2 =
    divides1 15 == [1, 3, 5, 15]

prop_divides0_divides1_0 x
    | x <= 0 = True
    | otherwise = (divides0 x == divides1 x)

is_prime :: Integer -> Bool
is_prime x
    | x < 0 = error "Number should be non negative"   
    | x == 0 = False
    | x == 1 = True
    | otherwise = (divides1 x) == [1, x]  

prop_is_prime_0 = 
    is_prime 0 == False

prop_is_prime_1 = 
    is_prime 1 == True

prop_is_prime_2 = 
    is_prime 2 == True

prop_is_prime_3 = 
    is_prime 4 == False

prop_is_prime_4 = 
    is_prime 27 == False

prop_is_prime_5 = 
    is_prime 179 == True

prop_is_prime_6 = 
    is_prime 997 == True

prop_is_prime_7 = 
    is_prime 1000 == False

-- Exercise 3
lengthString :: String -> Int
lengthString [] = 0
lengthString (x:xs) = 1 + lengthString xs

prefix :: String -> String -> Bool
prefix pr st
    | lengthString st < lengthString pr = False
    | otherwise = pr == take (lengthString pr) st 

prop_prefix_0 = 
    prefix "some" "another" == False

prop_prefix_1 = 
    prefix "some" "a" == False

prop_prefix_2 = 
    prefix "some" "some" == True

prop_prefix_3 = 
    prefix "sm" "some" == False

prop_prefix_4 = 
    prefix "" "some" == True

prop_prefix_5 = 
    prefix "" "" == True

prop_prefix_6 = 
    prefix "some" "" == False

substring :: String -> String -> Bool
substring sub st 
    | st == "" = False
    | otherwise = prefix sub st || substring sub (drop 1 st)

prop_substring_0 = 
    substring "some" "" == False

prop_substring_1 = 
    substring "some" "asomea" == True

prop_substring_2 = 
    substring "some" "bbbbbbbbbbbbbbbbbbbbbbsomeb" == True

prop_substring_3 = 
    substring "some" "somebbbbbbbbbbbbbbbbbbb" == True

prop_substring_4 = 
    substring "some" "some" == True

prop_substring_5 = 
    substring "some" "ome" == False

prop_substring_6 = 
    substring "" "ome" == True

main = do
    -- Exercise 1 tests
    quickCheck prop_average_0
    quickCheck prop_average_1
    quickCheck prop_average_2
    quickCheck prop_average_3

    -- Exercise 2 tests
    quickCheck prop_divides0_0
    quickCheck prop_divides0_1
    quickCheck prop_divides0_2

    quickCheck prop_divides1_0
    quickCheck prop_divides1_1
    quickCheck prop_divides1_2

    quickCheck prop_divides0_divides1_0

    quickCheck prop_is_prime_0
    quickCheck prop_is_prime_1
    quickCheck prop_is_prime_2
    quickCheck prop_is_prime_3
    quickCheck prop_is_prime_4
    quickCheck prop_is_prime_5
    quickCheck prop_is_prime_6
    quickCheck prop_is_prime_7

    -- Exercise 3 tests
    quickCheck prop_prefix_0
    quickCheck prop_prefix_1
    quickCheck prop_prefix_2
    quickCheck prop_prefix_3
    quickCheck prop_prefix_4
    quickCheck prop_prefix_5
    quickCheck prop_prefix_6

    quickCheck prop_substring_0
    quickCheck prop_substring_1
    quickCheck prop_substring_2
    quickCheck prop_substring_3
    quickCheck prop_substring_4
    quickCheck prop_substring_5
    quickCheck prop_substring_6

    putStrLn (show (substring "" "ome"))
