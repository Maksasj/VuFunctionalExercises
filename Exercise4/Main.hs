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
data Result a = OK a | Error String
  deriving (Show, Eq)

composeResult :: (a -> Result b) -> (b -> Result c) -> (a -> Result c)
composeResult f g = \x -> case f x of
    Error msg -> Error msg
    OK result -> g result

checkPositive :: Int -> Result Int
checkPositive n | n >= 0    = OK n
                | otherwise = Error "Not positive"

addOne :: Int -> Result Int
addOne n = OK (n + 1)

prop_composeResult_0 = 
    composeResult checkPositive addOne 5 == OK 6

prop_composeResult_1 = 
    composeResult checkPositive addOne (-1) == Error "Not positive"

limitValue :: Int -> Result Int
limitValue n | n < 10    = OK n
             | otherwise = Error "Too large"

prop_composeResult_2 = 
    composeResult addOne limitValue 9 == Error "Too large"

-- Exercise 5

-- Exercise 6

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
