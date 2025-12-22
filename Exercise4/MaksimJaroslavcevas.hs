module Main where

import Test.QuickCheck;
import Prelude hiding ((<*>));

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
data Expr a = Lit a | EVar Var | Op (Ops a) [Expr a]
type Ops a = [a] -> a
type Var = Char

type Valuation a = Var -> a

eval :: Valuation a -> Expr a -> a
eval _ (Lit x) = x
eval valuation (EVar v) = valuation v
eval valuation (Op f exprs) = f (map (eval valuation) exprs)

val 'x' = 2; val 'y' = 5

prop_eval_0 = 
    eval val (Lit 10) == 10

prop_eval_1 = 
    eval val (EVar 'x') == 2

prop_eval_2 = 
    eval val (Op sum [EVar 'x', Lit 5, EVar 'y']) == 12

prop_eval_3 = 
    eval val (Op product [EVar 'y', Op sum [EVar 'x', Lit 3]]) == 25

-- Exercise 3
type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (=="")

char :: Char -> RegExp
char ch = (==[ch])

(|||) :: RegExp -> RegExp -> RegExp
e1 ||| e2 = \x -> e1 x || e2 x

splits :: String -> [(String, String)]
splits xs = [splitAt n xs | n <- [0 .. length xs]]

(<*>) :: RegExp -> RegExp -> RegExp
e1 <*> e2 = \x ->
    or [e1 y && e2 z | (y,z) <- splits x]

star :: RegExp -> RegExp
star p = epsilon ||| (p <*> star p)

option :: RegExp -> RegExp
option p = p ||| epsilon

plus :: RegExp -> RegExp
plus p = p <*> star p

prop_option_0 =
    option (char 'a') "" == True

prop_option_1 =
    option (char 'a') "a" == True

prop_option_2 =
    option (char 'a') "" == True

prop_option_3 =
    option (char 'a') "aa" == False

prop_option_4 =
    option (char 'a') "b" == False

prop_option_5 =
    option (char 'b') "bab" == False

prop_plus_0 = 
    plus (char 'a') "" == False

prop_plus_1 = 
    plus (char 'a') "a" == True

prop_plus_2 = 
    plus (char 'a') "aaa" == True

prop_plus_3 = 
    plus (char 'a') "" == False

prop_plus_4 = 
    plus (char 'a') "ab" == False

-- Exercise 4
data ResultM a = OK a | Error String
  deriving (Show, Eq)

composeResult :: (a -> ResultM b) -> (b -> ResultM c) -> (a -> ResultM c)
composeResult f g = \x -> case f x of
    Error msg -> Error msg
    OK result -> g result

checkPositive :: Int -> ResultM Int
checkPositive n | n >= 0    = OK n
                | otherwise = Error "Not positive"

addOne :: Int -> ResultM Int
addOne n = OK (n + 1)

prop_composeResult_0 = 
    composeResult checkPositive addOne 5 == OK 6

prop_composeResult_1 = 
    composeResult checkPositive addOne (-1) == Error "Not positive"

limitValue :: Int -> ResultM Int
limitValue n | n < 10    = OK n
             | otherwise = Error "Too large"

prop_composeResult_2 = 
    composeResult addOne limitValue 9 == Error "Too large"

-- Exercise 5
primes :: [Integer]
primes = sieve [2 ..]

sieve (x:xs) =
    x : sieve [y | y <- xs, y `mod` x > 0]

goldbach :: Integer -> Bool
goldbach n = and [ isSumOfTwoPrimes k | k <- evens ]
  where
    evens = [4, 6 .. n]
    
    isSumOfTwoPrimes k = not (null [ (p1, p2) | p1 <- ps, p2 <- ps, p1 + p2 == k ])
      where 
        ps = takeWhile (< k) primes

prop_goldbach_0 = 
    goldbach 4 == True

prop_goldbach_1 = 
    goldbach 10 == True

prop_goldbach_2 = 
    goldbach 20 == True

prop_goldbach_3 = 
    goldbach 2 == True

-- Exercise 6
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f seed = Cons seed (streamIterate f (f seed))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)

takeS :: Int -> Stream a -> [a]
takeS n s = take n (streamToList s)

prop_stream_0 = takeS 5 (streamIterate (*2) 1) == [1, 2, 4, 8, 16]

prop_stream_1 = 
    let s1 = streamIterate id 1
        s2 = streamIterate id 2
    in takeS 6 (streamInterleave s1 s2) == [1, 2, 1, 2, 1, 2]

prop_stream_2 = head (streamToList (Cons 100 (streamIterate id 0))) == 100

main = do
    -- Exercise 1
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

    -- Exercise 2
    quickCheck prop_eval_0
    quickCheck prop_eval_1
    quickCheck prop_eval_2
    quickCheck prop_eval_3

    -- Exercise 3
    quickCheck prop_option_0
    quickCheck prop_option_1
    quickCheck prop_option_2
    quickCheck prop_option_3
    quickCheck prop_option_4
    quickCheck prop_option_5

    quickCheck prop_plus_0
    quickCheck prop_plus_1
    quickCheck prop_plus_2
    quickCheck prop_plus_3
    quickCheck prop_plus_4

    -- Exercise 4
    quickCheck prop_composeResult_0
    quickCheck prop_composeResult_1
    quickCheck prop_composeResult_2

    -- Exercise 5
    quickCheck prop_goldbach_0
    quickCheck prop_goldbach_1
    quickCheck prop_goldbach_2
    quickCheck prop_goldbach_3

    -- Exercise 6
    quickCheck prop_stream_0
    quickCheck prop_stream_1
    quickCheck prop_stream_2