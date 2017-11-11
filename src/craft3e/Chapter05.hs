{- #############################################################################
   Sample code from:
      Simon Thompson - Haskell: the Craft of Functional Programming, 2011
      ++++ Addison-Wesley ++++
      http://www.haskellcraft.com/craft3e/Home.html
############################################################################# -}

module Craft3e.Chapter05 where

import Data.Char
import Test.QuickCheck

data ShopItem = ShopItem { name :: String, price :: Int }
    deriving (Show, Read, Eq)

type Basket = [ShopItem]

salt = ShopItem "Salt: 1Kg" 139

basket :: Basket
basket = [ ShopItem "Salt: 1Kg" 139
            , ShopItem "Plain crisps" 25
            , ShopItem "Plain crisps" 25
            ]

minAndMax :: (Ord a) => a -> a -> (a, a)
minAndMax m n | m >= n    = (n, m)
                | otherwise = (m, n)

minAndMax_prop1 m n = let (min,max) = minAndMax m n
                        in (min == m || min == n) && (max == m || max == n)

minAndMax_prop2 m n = let (min,max) = minAndMax m n
                        in min <= max

addPair :: (Num a) => (a, a) -> a
addPair (m, n) = m + n

addPair_prop1 m n = addPair (m, n) == m + n

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fastFib :: Integer -> Integer
fastFib n | n < 0 = error "Prelude.fib': undefined for negative values"
fastFib n = fibIter n (0, 1)
    where fibIter 0 (fib1, fib2) = fib1
        fibIter n (fib1, fib2) = fibIter (n - 1) (fib2, fib1 + fib2)

fib_prop n = n >= 0 && n < 20 ==> fib n == fastFib n

{-
    Ex 5.1 it returns the maximum of two integers, together with the number
    of times it occurs.
-}
maxOccurs :: Integer -> Integer -> (Integer,Integer)
maxOccurs m n | m == n    = (m, 2)
                | m > n     = (m, 1)
                | otherwise = (n, 1)

{-
    It returns the maximum of three integers, together with the number
    of times it occurs.
-}
maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs m n p | m == n && n == p = (m, 3)
maxThreeOccurs m n p = let (max,  occ)  = maxOccurs m n
                            (max', occ') = maxOccurs max p
                        in  (max', occ')

maxThreeOccurs_findMaxProp m n p = let (max, _ ) = maxThreeOccurs m n p
                                    in max == findMax
                                    where findMax = m `max` n `max` p

maxThreeOccurs_maxOccProp m n p = let (max, occ) = maxThreeOccurs m n p
                                    in occ == findOcc max
    where findOcc max = (count max m) + (count max n) + (count max p)
            count max x = if x == max then 1 else 0

{-
    Ex 5.2 it puts the element of a triple of three integers into ascending order.
-}
orderTriple :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
orderTriple (m, n, p) = let (max, (x, y)) = maxThree m n p
                        in if x >= y then (max, x, y)
                            else           (max, y, x)
    where maxThree x y z | x >= y && x >= z = (x, (y, z))
                        | y >= z           = (y, (x, z))
                        | otherwise        = (z, (x, y))

orderTriple_prop m n p = let (x, y, z) = orderTriple (m, n, p)
                            in (x >= y && x >= z) &&
                            (y <= x && y >= z) &&
                            (z <= x && z <= y)


data Shape = Circle Float | Rectangle Float Float
    deriving (Show, Eq, Ord)

isRound :: Shape -> Bool
isRound (Circle _)      = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r)      = pi * r ^ 2
area (Rectangle h w) = h * w

perimeter :: Shape -> Float
perimeter (Circle r)      = 2 * pi * r
perimeter (Rectangle h w) = 2 * (h + w)

type Name = String
type Age  = Int
data People = Person Name Age
    deriving (Eq, Show)

showPerson :: People -> String
showPerson (Person n a) = "Person(name = " ++ n ++ ", age = " ++ show a ++ ")"

{- Ex 5.18 -}
doubleAll :: [Integer] -> [Integer]
doubleAll xs = [2 * x | x <- xs]

{- Ex 5.19 -}
capitalize :: String -> String
capitalize cs = [changeCase c | c <- cs]
    where changeCase ch = if isLower ch then toUpper ch else ch

capitalizeLetters :: String -> String
capitalizeLetters cs = [changeCase c | c <- cs , isAlpha c]
    where changeCase ch = if isLower ch then toUpper ch else ch

capitalize_prop1 :: String -> Bool
capitalize_prop1 str = let out = capitalize str
                        in allCapitals out
                        where allCapitals xs = all isUpper $
                                            filter isAlpha $
                                            filter isAscii xs

{- Ex 5.20 -}
divisors :: Integer -> [Integer]
divisors n = [d | d <- [1..n] , n `mod` d == 0]

divisors_prop :: Integer -> Bool
divisors_prop n = let divs = divisors n
                    in all isDivisible divs
                where isDivisible x = n `mod` x == 0

{- Ex 5.21 -}
matches :: Integer -> [Integer] -> [Integer]
matches _ [] = []
matches x' (x:xs)
    | x == x'    = x : matches x' xs
    | otherwise  = matches x' xs

matches' :: Integer -> [Integer] -> [Integer]
matches' x xs = [el | el <- xs , el == x]

{- Ex 5.22 -}
mkString :: [String] -> String
mkString [] = ""
mkString (x:xs) = x ++ "\n" ++ (mkString xs)

onSeparateLines :: [String] -> String
onSeparateLines xs = mkString xs

{-
    Ex 5.23: takes a string and an integer n. The result is n copies of the
    string joined together. If n <= 0, the result should be "".
-}
duplicate :: String -> Integer -> String
duplicate _   n | n <= 0 = ""
duplicate str n = str ++ duplicate str (n - 1)

{- Ex 5.24 -}
pushRight :: String -> String
pushRight str = let linelength = 12
                    in padRight (linelength - length str) ++ str
                where padRight sp | sp <= 0   = ""
                                | otherwise = ' ' : padRight (sp - 1)

padRight :: Int -> String -> String
padRight spaces str | length str >= spaces = str
                    | spaces == 0          = str
                    | otherwise            = ' ' : padRight (spaces - 1) str

{- Ex 5.26 -}
fibTable :: Integer -> String
fibTable n = header ++ "\n" ++ (onSeparateLines $ map printFib [(x, fastFib x) | x <- [0..n]])
    where printFib (x, fibx) = (padNum) 3 x ++ (padNum 12 fibx)
        header             = " n " ++ padRight 12 "fib n"
        padNum sp n        = padRight sp (show n)