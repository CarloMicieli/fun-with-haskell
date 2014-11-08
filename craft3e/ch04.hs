{- #############################################################################
   Sample code from:
      Simon Thompson - Haskell: the Craft of Functional Programming, 2011
      ++++ Addison-Wesley ++++
      http://www.haskellcraft.com/craft3e/Home.html
############################################################################# -}

module Craft3e.Chapter4 where

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

sumFacs :: Integer -> Integer
sumFacs n = sum $ map (fac) [0..n]

{- Ex 4.19 -}
mult :: Integer -> Integer -> Integer
mult _ 0 = 0
mult 0 _ = 0
mult m n = fixSign $ mul m (abs n)
    where mul res 1     = res
          mul res times = mul (res + m) (times - 1)
          fixSign res   = if signum n == 1 then res else negate res

mult_prop1 :: Integer -> Integer -> Bool
mult_prop1 m n = m `mult` n == m * n

{- Ex 4.20: returns the integer square root of a positive integer n -}
intSquareRoot :: Integer -> Integer
intSquareRoot n | n < 0 = error "Prelude.intSquareRoot: undefined for negative numbers"
intSquareRoot n = let squareList = [(x, x * x) | x <- [n, (n - 1)..1] , x * x <= n]
                   in fst $ head $ squareList

intSquareRoot' :: Integer -> Integer
intSquareRoot' n = root 1
    where root r = let sq = (r + 1) * (r + 1)
                    in if sq > n then r
                       else           root (r + 1)

intSquareRoot_prop1 :: (Positive Integer) -> Bool
intSquareRoot_prop1 (Positive n) = intSquareRoot n == intSquareRoot' n

{- Ex 4.21: it finds the maximum of the values f 0, f 1, ... , f n -}
maxValue :: (Integer -> Integer) -> Integer -> Integer
maxValue f 0 = f 0
maxValue f n = findMax (f n) (n - 1)
    where findMax max' 0 = max max' (f 0)
          findMax max' x = findMax (max max' (f x)) (x - 1)

f :: Integer -> Integer
f 0 = 0
f 1 = 44
f 2 = 17
f 3 = 42
f _ = -1

g :: Integer -> Integer
g x = x + 1

{-
  Ex 4.22: given a function f of type Integer -> Integer give a recursive
  definition of a function of type Integer -> Bool which on input n returns
  True if one or more of the values f 0, f 1, ..., f n is zero or False
  otherwise.
-}
withZero :: (Integer -> Integer) -> Integer -> Bool
withZero f 0 = f 0 == 0
withZero f n = findZero (yieldZero f n) (n - 1)
    where yieldZero f i = f i == 0
          findZero True _ = True
          findZero _    x = let isZeroAtX = yieldZero f x
                             in if x == 0 then isZeroAtX
                                else           findZero isZeroAtX (x - 1)

fib :: Integer -> Integer
fib n
    | n == 0      = 0
    | n == 1      = 1
    | n > 1       = fib (n - 2) + fib (n - 1)

fib' :: Integer -> Integer
fib' 0 = 0
fib' n = fibIter n (0, 1)
    where fibIter 0 (val, _)       = val
          fibIter i (val, nextVal) = fibIter (i - 1) (nextVal, val + nextVal)

remainder :: Integer -> Integer -> Integer
remainder m n
    | m < n     = m
    | otherwise = remainder (m - n) n

divide :: Integer -> Integer -> Integer
divide m n
    | m < n     = 0
    | otherwise = 1 + divide (m - n) n

divide' :: Integer -> Integer -> Integer
divide' m n = div 0 m
    where div res rem | rem < n   = res
                      | otherwise = div (res + 1) (rem - n)

{- Ex 4.31 -}
hcf :: Integer -> Integer -> Integer
hcf = undefined

{- Ex 4.32 -}
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
