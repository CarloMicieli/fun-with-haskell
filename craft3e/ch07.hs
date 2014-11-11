{- #############################################################################
   Sample code from:
      Simon Thompson - Haskell: the Craft of Functional Programming, 2011
      ++++ Addison-Wesley ++++
      http://www.haskellcraft.com/craft3e/Home.html
############################################################################# -}

module Craft3e.Chapter7 where

import Test.QuickCheck
import Data.Char
import Prelude hiding ( product
                      , elem
                      , and
                      , or
                      , concat
                      , (++))

numbers :: [Integer]
numbers = [42, 15, 78, 9, 1]

singleton :: [Integer]
singleton = [42]

xs :: [Integer]
xs = [4, 2, 1, 3, 2, 3]

{-
  Ex 7.1: returns the first integer in a list plus one, if there is one, and
  returns zero otherwise.
-}
firstPlusOne :: [Integer] -> Integer
firstPlusOne []    = 0
firstPlusOne (x:_) = x + 1

{-
  Ex 7.2: adds together the first two integers in a list, if a list contains
  at least two elements; returns the head element if the list contains one,
  and returns zero otherwise.
-}
addFirstTwo :: [Integer] -> Integer
addFirstTwo []       = 0
addFirstTwo [x]      = x
addFirstTwo (x:y:ys) = x + y

{- Ex 7.3 -}
firstPlusOne' :: [Integer] -> Integer
firstPlusOne' xs = sum $ take 1 (map (+1) xs)

addFirstTwo' :: [Integer] -> Integer
addFirstTwo' xs = sum $ take 2 xs

{- Ex 7.4: returns the first digit (0 - 9) in the string -}
firstDigit :: String -> Char
firstDigit ""     = '\0'
firstDigit (c:cs) | isDigit c = c
                  | otherwise = firstDigit cs

{- Ex 7.5 -}
product :: [Integer] -> Integer
product []     = 1
product (x:xs) = x * product xs

{- Ex 7.6 -}
and :: [Bool] -> Bool
and []        = True
and (False:_) = False
and (True:xs) = and xs

or :: [Bool] -> Bool
or []     = False
or (x:xs) | x == True = True
          | otherwise = or xs

(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x : (xs ++ ys)

concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

elem :: Integer -> [Integer] -> Bool
elem _ []      = False
elem x' (x:xs) | x == x' = True
               | otherwise = elem x' xs

doubleAll :: [Integer] -> [Integer]
doubleAll []     = []
doubleAll (x:xs) = (x * 2) : doubleAll xs

doubleAll' :: [Integer] -> [Integer]
doubleAll' xs = [2 * x | x <- xs]

selectEven :: [Integer] -> [Integer]
selectEven (x:y:ys) = y : selectEven ys
selectEven _        = []

selectEven' :: [Integer] -> [Integer]
selectEven' xs = [x | (i,x) <- zip [1..] xs , even i]

iSort :: [Integer] -> [Integer]
iSort []     = []
iSort (x:xs) = ins x (iSort xs)

ins :: Integer -> [Integer] -> [Integer]
ins x []          = [x]
ins x rest@(y:ys) | x <= y    = x : rest
                  | otherwise = y : (ins x ys)

{- Ex 7.8 -}
elemNum :: Integer -> [Integer] -> Integer
elemNum _ []     = 0
elemNum x (y:ys) = (if x == y then 1 else 0) + elemNum x ys

{-
  Ex 7.9 returns the list of elements of xs which occur exactly once.
  For example, unique [4, 2, 1, 3, 2, 3] is [4, 1]
  The function is O(n^2)
-}
unique :: [Integer] -> [Integer]
unique []     = []
unique (x:xs) = let rest = [y | y <- xs , y /= x]
                 in if isDupl x then     unique rest
                    else             x : unique rest
              where isDupl x = any (== x) xs

{- Ex 7.11 -}
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

unzip' :: [(a, b)] -> ([a], [b])
unzip' []         = ([], [])
unzip' ((x,y):ys) = let pair = unzip' ys
                     in (x:fst pair, y:snd pair)

{- Ex 7.12 -}
minAndMax :: [Integer] -> (Integer, Integer)
minAndMax []     = error "Empty list"
minAndMax [x]    = (x, x)
minAndMax (x:xs) = let (min', max') = minAndMax xs
                    in (min x min', max x max')

minAndMax' :: [Integer] -> (Integer, Integer)
minAndMax' []   = error "Prelude.minAndMax: empty list"
minAndMax' [x]  = (x, x)
minAndMax' xs   = let (y:ys) = iSort xs
                   in (y, last ys)
