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

-- Exercise 4
removeFirst :: [Integer] -> Integer -> [Integer]
removeFirst [] _ = []
removeFirst (y:ys) x
    | x == y = ys
    | otherwise = y:(removeFirst ys x)

prop_removeFirst_0 =
    removeFirst [] 10 == []

prop_removeFirst_1 = 
    removeFirst [10] 10 == []

prop_removeFirst_2 =
    removeFirst [10, 10] 10 == [10]

prop_removeFirst_3 =
    removeFirst [1, 2, 3, 4, 5, 5, 6] 5 == [1, 2, 3, 4, 5, 6]

prop_removeFirst_4 = 
    removeFirst [5, 2, 3, 4, 5, 5, 6] 5 == [ 2, 3, 4, 5, 5, 6]

permut :: [Integer] -> [Integer] -> Bool
permut [] [] = False
permut [_] [] = False
permut [] [_] = False
permut [a] [b] = a == b
permut x y 
    | length(x) == length(y) = permut (drop 1 x) (removeFirst y (head x))
    | otherwise = False

prop_permut_0 =
    permut [] [] == False

prop_permut_1 =
    permut [1] [1] == True

prop_permut_2 =
    permut [1] [2] == False

prop_permut_3 =
    permut [1, 1] [2] == False

prop_permut_4 =
    permut [1, 1] [1, 1] == True

prop_permut_5 =
    permut [2, 1] [1, 2] == True

prop_permut_6 =
    permut [1, 2, 3, 4, 5, 6] [6, 5, 4, 3, 2, 1] == True

prop_permut_7 =
    permut [1, 2, 3, 4, 5, 6] [6, 5, 4, 3, 7, 1] == False

prop_permut_8 =
    permut [1, 2, 3, 4, 5, 6] [6, 5, 4, 3, 1] == False

prop_permut_9 =
    permut [4, 5, 6] [6, 5, 4, 3, 1] == False

-- Exercise 5
isLowerCaseAlpha :: Char -> Bool
isLowerCaseAlpha x = x >= 'a' && x <= 'z'

isUpperCaseAlpha :: Char -> Bool
isUpperCaseAlpha x = x >= 'A' && x <= 'Z'

isAlphabet :: Char -> Bool
isAlphabet x = isLowerCaseAlpha x || isUpperCaseAlpha x

toUpper :: Char -> Char
toUpper x
    | isLowerCaseAlpha x = toEnum ((fromEnum x) - 32)
    | isUpperCaseAlpha x = x
    | otherwise = error "Character is not alphabetical character"

capitalise :: String -> String
capitalise s = [toUpper c | c <- s, isAlphabet c] 

prop_capitalise_0 =
    capitalise "some" == "SOME"

prop_capitalise_1 =
    capitalise "14124some141241" == "SOME"

prop_capitalise_2 =
    capitalise "S1O2M3E4" == "SOME"

prop_capitalise_3 =
    capitalise "someS1O2M3E4" == "SOMESOME"

prop_capitalise_4 =
    capitalise "s" == "S"

prop_capitalise_5 =
    capitalise "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" == "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- Exercise 6
addToKey :: [(String,Float)] -> (String,Float) -> [(String,Float)]
addToKey [] x = [x]
addToKey [(y0, p0)] (y1, p1)
    | y0 == y1 = [(y0, p0 + p1)]
    | otherwise = [(y0, p0), (y1, p1)]
addToKey ((y0, p0):xs) (y1, p1)
    | xs == [] = []
    | y0 == y1 = (y0, p0 + p1):xs  
    | otherwise = (y0, p0):(addToKey xs (y1, p1))

prop_addToKey_0 =
    addToKey [] ("a", 1) == [("a", 1)] 

prop_addToKey_1 =
    addToKey [("a", 2)] ("b", 1) == [("a", 2), ("b", 1)] 

prop_addToKey_2 =
    addToKey [("b", 1), ("c", -1), ("d", 3.3)] ("a", 2) == [("b", 1), ("c", -1), ("d", 3.3), ("a", 2)] 

prop_addToKey_3 =
    addToKey [("a", 2), ("b", 1), ("c", 1)] ("b", -5) == [("a", 2), ("b", -4), ("c", 1)] 

itemTotal :: [(String,Float)] -> [(String,Float)]
itemTotal [] = []
itemTotal [x] = [x]
itemTotal (x:xs) = (addToKey (itemTotal xs) x) 

prop_itemTotal_0 =
    itemTotal [] == []

prop_itemTotal_1 =
    itemTotal [("a", 1)] == [("a", 1)]

prop_itemTotal_2 =
    itemTotal [("a", 1), ("a", 1)] == [("a", 2)]

prop_itemTotal_3 =
    itemTotal [("a", 1), ("b", 3), ("a", 1)] == [("a", 2), ("b", 3 )]

prop_itemTotal_4 =
    itemTotal [("a", 1), ("b", 2), ("b", 3), ("a", 1)] == [("a", 2), ("b", 5)]

prop_itemTotal_5 =
    itemTotal [("c", 3.3), ("a", 1), ("b", 2), ("b", 3), ("a", 1)] == [("a", 2), ("b", 5), ("c", 3.3)]

itemDiscount :: String -> Integer -> [(String,Float)] -> [(String,Float)]
itemDiscount _ _ [] = []
itemDiscount item disc [(y0, p0)]
    | item == y0 = [(y0, p0 * (fromIntegral disc / 100))]
    | otherwise = [(y0, p0)]
itemDiscount item disc ((y0, p0):xs)
    | item == y0 = (y0, p0 * (fromIntegral disc / 100)):(itemDiscount item disc xs)
    | otherwise = (y0, p0):(itemDiscount item disc xs)

prop_itemDiscount_0 =
    itemDiscount "a" 50 [] == [] 

prop_itemDiscount_1 =
    itemDiscount "a" 50 [("a", 2), ("a", 100)] == [("a", 1), ("a", 50)] 

prop_itemDiscount_2 =
    itemDiscount "b" 50 [("a", 2), ("b", 100)] == [("a", 2), ("b", 50)] 

prop_itemDiscount_3 =
    itemDiscount "c" 50 [("a", 2), ("b", 100)] == [("a", 2), ("b", 100)] 

prop_itemDiscount_4 =
    itemDiscount "a" 20 [("a", 2), ("b", 100)] == [("a", 0.4), ("b", 100)] 

prop_itemDiscount_5 =
    itemDiscount "a" 20 [("c", 100), ("a", 2), ("b", 100)] == [("c", 100), ("a", 0.4), ("b", 100)] 

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

    -- Exercise 4 tests
    quickCheck prop_removeFirst_0
    quickCheck prop_removeFirst_1
    quickCheck prop_removeFirst_2
    quickCheck prop_removeFirst_3
    quickCheck prop_removeFirst_4

    quickCheck prop_permut_0
    quickCheck prop_permut_1
    quickCheck prop_permut_2
    quickCheck prop_permut_3
    quickCheck prop_permut_4
    quickCheck prop_permut_5
    quickCheck prop_permut_6
    quickCheck prop_permut_7
    quickCheck prop_permut_8
    quickCheck prop_permut_9
    
    -- Exercise 5 tests
    quickCheck prop_capitalise_0
    quickCheck prop_capitalise_1
    quickCheck prop_capitalise_2
    quickCheck prop_capitalise_3
    quickCheck prop_capitalise_4
    quickCheck prop_capitalise_5

    -- Exercise 6 tests
    quickCheck prop_addToKey_0
    quickCheck prop_addToKey_1
    quickCheck prop_addToKey_2
    quickCheck prop_addToKey_3

    quickCheck prop_itemTotal_0
    quickCheck prop_itemTotal_1
    quickCheck prop_itemTotal_2
    quickCheck prop_itemTotal_3
    quickCheck prop_itemTotal_4
    quickCheck prop_itemTotal_5

    quickCheck prop_itemDiscount_0
    quickCheck prop_itemDiscount_1
    quickCheck prop_itemDiscount_2
    quickCheck prop_itemDiscount_3
    quickCheck prop_itemDiscount_4
    quickCheck prop_itemDiscount_5
