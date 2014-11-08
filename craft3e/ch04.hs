{- #############################################################################
   Sample code from:
      Simon Thompson - Haskell: the Craft of Functional Programming, 2011
      ++++ Addison-Wesley ++++
      http://www.haskellcraft.com/craft3e/Home.html
############################################################################# -}

module Craft3e.Chapter04 where

import Data.List
import Test.QuickCheck

xs :: [Integer]
xs = [42, 34, 23, 15, 88]

ys :: [Integer]
ys = [42, 42, 15, 88, 42]

zs :: [Integer]
zs = [34, 25, 42, 34]

{- Ex 4.1: the function which returns the maximum of four integers -}
maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour m n p q = max (max m n) (max p q)

{- Ex 4.2 -}
middle :: (Ord a) => a -> a -> a -> a
middle m n p
  | between m n p || between m p n = m
  | between n m p || between n p m = n
  | otherwise                      = p

middle_prop1 :: Integer -> Integer -> Integer -> Bool
middle_prop1 m n p = let mid = middle m n p
                      in between mid n p || between mid p n

between :: (Ord a) => a -> a -> a -> Bool
between v min max = min <= v && v <= max

between' :: (Ord a) => a -> a -> a -> Bool
between' v min max = weakAscOrder min v max

between_prop1 :: Integer -> Integer -> Integer -> Bool
between_prop1 v m n = between v m n == between' v m n

{-
  Returns 'True' exactly when m, n  and p are in weak ascending order
  that is the sequence does not go down at any point.

  weakAscOrder 2 3 3 ~~> True
-}
weakAscOrder :: (Ord a) => a -> a -> a -> Bool
weakAscOrder m n p = m <= n && n <= p

weakAscOrder' :: (Ord a) => a -> a -> a -> Bool
weakAscOrder' m n p = let xs = [m, n, p] in xs == sort xs

weakAscOrder_prop1 :: Integer -> Integer -> Integer -> Bool
weakAscOrder_prop1 m n p = weakAscOrder m n p == weakAscOrder' m n p

{- Ex 4.3 -}
howManyEquals :: (Eq a) => a -> a -> a -> Int
howManyEquals m n p
  | allEquals    = 3
  | allDifferent = 0
  | otherwise    = 2
  where allEquals    = m == n && n == p
        allDifferent = m /= n && n /= p && m /= p

{- Ex 4.4 -}
howManyOfFourEquals :: Integer -> Integer -> Integer -> Integer -> Integer
howManyOfFourEquals m n p q = fromIntegral $ howManyEqualsList [m, n, p, q]

howManyEqualsList :: (Show a, Ord a) => [a] -> Int
howManyEqualsList xs = case occurrences of
      []            -> 0
      [(_,x)]       -> x
      [(x,_),(y,_)] -> error ("Prelude.howMany: " ++ show x ++ " and " ++ show y ++ " have duplicates")
  where groupDuplicate = group . sort
        occurrences = [(head ys, length ys) | ys <- groupDuplicate xs , length ys > 1]

triArea :: Float -> Float -> Float -> Float
triArea a b c
  | possible  = sqrt(s * (s - a) * (s - b) * (s - c))
  | otherwise = 0
  where s        = (a + b + c) / 2
        possible = (a > 0 && a < b + c) && (b > 0 && b < a + c) && (c > 0 && c < a + b)

sumSquares :: Integer -> Integer -> Integer
sumSquares n m = sqN + sqM
  where sqN = n * n
        sqM = m * m

isOdd, isEven :: Int -> Bool
isOdd n
  | n == 0    = False
  | n < 0     = error "Prelude.isOdd: undefined for negative numbers"
  | otherwise = isEven (n - 1)

isEven n
  | n == 0    = True
  | n < 0     = error "Prelude.isEven: undefined for negative numbers"
  | otherwise = isOdd (n - 1)

isEven_prop1 :: (Positive Int) -> Bool
isEven_prop1 (Positive n) = even n == isEven n

isOdd_prop1 :: (Positive Int) -> Bool
isOdd_prop1 (Positive n) = odd n == isOdd n

{- Ex 4.17 -}
rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
  | m <= n    = product [m .. n]
  | otherwise = 0

{- Ex 4.18 -}
fac :: Integer -> Integer
fac n
  | n == 0    = 1
  | n > 0     = n * fac (n - 1)
  | otherwise = error "Prelude.fac: undefined for negative numbers"

fac' :: Integer -> Integer
fac' = rangeProduct 1


power2 :: Integer -> Integer
power2 n   | n < 0 = error "Prelude.power2: undefined for negative numbers"
power2 0   = 1
power2 1   = 2
power2 n
  | odd n  = 2 * halfPow2 * halfPow2
  | even n =     halfPow2 * halfPow2
  where halfN    = n `div` 2
        halfPow2 = power2 halfN

power2_prop1 :: (Positive Integer) -> Bool
power2_prop1 (Positive n) = power2 n == 2 ^ n

sumFacs :: Integer -> Integer
sumFacs n = sum $ map (fac) [0..n]
