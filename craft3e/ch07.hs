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
                      , and
                      , or
                      , concat
                      , getLine
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

elem' :: Integer -> [Integer] -> Bool
elem' _ []      = False
elem' x' (x:xs) | x == x' = True
                | otherwise = elem' x' xs

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
unzip' ((x,y):ys) = let (f, s) = unzip' ys
                     in (x : f, y : s)

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

{- Ex 7.16 -}
iSort' :: (Integer -> [Integer] -> [Integer]) -> [Integer] -> [Integer]
iSort' _ []     = []
iSort' f (x:xs) = f x (iSort' f xs)

ins' :: Integer -> [Integer] -> [Integer]
ins' x []        = [x]
ins' x (y:ys)    | x >= y     = x : (y : ys)
                 | otherwise  = y : (ins' x ys)

ins'' :: Integer -> [Integer] -> [Integer]
ins'' x []       = [x]
ins'' x l@(y:ys) | x == y    = l
                 | x <= y    = x : l
                 | otherwise = y : (ins'' x ys)

{- Ex 7.19 -}
sortPairs :: [(Integer, Integer)] -> [(Integer, Integer)]
sortPairs []     = []
sortPairs (x:xs) = ins x (sortPairs xs)
  where ins x       []     = [x]
        ins x@(k,v) (y@(k',_):ys) | k <= k'   = x : y : ys
                                  | otherwise = y : (ins x ys)

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ []     = ([], [])
partition p (x:xs) = let (t, f) = partition p xs
                      in if (p x) then (x : t, f)
                         else          (t, x : f)

qSort :: [Integer] -> [Integer]
qSort []         = []
qSort (pivot:xs) = let (left, right) = partition (<=pivot) xs
                    in (qSort left) ++ [pivot] ++ (qSort right)

{- Ex 7.20 -}
drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' n l@(x:xs) | n <= 0    = l
                 | n > 0     = drop' (n - 1) xs

drop_prop :: Int -> [Integer] -> Bool
drop_prop n xs = drop n xs == drop' n xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ []       = ([], [])
splitAt' n l@(x:xs) | n <= 0    = ([], l)
                    | otherwise = let (f, s) = splitAt' (n - 1) xs
                                   in (x : f, s)

splitAt_prop :: Int -> [Integer] -> Bool
splitAt_prop n xs = splitAt n xs == splitAt' n xs

{- Ex 7.21 -}
take' :: Int -> [a] -> [a]
take' n (x:xs) | n > 0 = x : take' (n - 1) xs
take' _ _      = []

{- Ex 7.22 -}
zip' :: ([a], [b]) -> [(a, b)]
zip' ((x:xs),(y:ys)) = (x,y) : zip' (xs, ys)
zip' _               = []

zipAndUnzip_prop :: [Integer] -> [Integer] -> Property
zipAndUnzip_prop xs ys = (length xs == length ys) ==>
                         let ps = (xs, ys)
                          in unzip' (zip' ps) == ps

{- Ex 7.23 -}
zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : (zip3' xs ys zs)
zip3' _      _      _      = []

zip3'' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3'' xs ys zs = map (\ ((x,y),z) -> (x,y,z)) $ zip (zip xs ys) zs

{- Ex 7.24 -}
qSort' :: [Integer] -> [Integer]
qSort' []         = []
qSort' (pivot:xs) = let (l, r) = partition (<= pivot) xs
                     in (qSort' r) ++ [pivot] ++ (qSort' l)

qSort'' :: [Integer] -> [Integer]
qSort'' [] = []
qSort'' (pivot:xs) = (qSort'' l) ++ [pivot] ++ (qSort'' r)
  where l = [x | x <- xs , x < pivot]
        r = [x | x <- xs , x > pivot]

{- Ex 7.25 -}
sublist :: Eq a => [a] -> [a] -> Bool
sublist []      _     = True
sublist _       []    = False
sublist (x:xs) (y:ys) = if (x == y)
                        then sublist xs ys
                        else sublist (x:xs) ys

subseq :: Eq a => [a] -> [a] -> Bool
subseq [] _  = True
subseq _  [] = False
subseq xs ys = find xs ys False
  where find  _      []    found = found
        find  []     _     found = found
        find (h:hs) (s:ss) found = if (h == s)
                                   then find hs ss True
                                   else find xs ss False


{- Example: text processing -}
whitespace :: [Char]
whitespace = ['\n', '\t', ' ']

isWhitespace :: Char -> Bool
isWhitespace = \ch -> elem ch whitespace

getWord :: String -> String
getWord []     = []
getWord (c:cs) = if isWhitespace c
                 then ""
                 else c : getWord cs

dropWord :: String -> String
dropWord []     = []
dropWord (c:cs) = if isWhitespace c
                  then c : cs
                  else dropWord cs

dropSpace :: String -> String
dropSpace []     = []
dropSpace (c:cs) | isWhitespace c = dropSpace cs
                 | otherwise      = c : cs

type Word = String
type Line = [Word]

splitWords :: String -> [Word]
splitWords "" = []
splitWords xs = let str = dropSpace xs
                 in (getWord str) : splitWords (dropSpace $ dropWord str)

getLine :: Int -> [Word] -> Line
getLine _   []     = []
getLine 0   _      = []
getLine len (w:ws) = let wordLength = length w
                         remaining = len - wordLength
                      in if remaining <= 0
                         then []
                         else w : getLine remaining ws

{- Ex 7.27 -}
dropLine :: Int -> [Word] -> Line
dropLine _    []    = []
dropLine 0    ws    = ws
dropLine len (w:ws) = let wordLength = length w
                          remaining = len - wordLength
                       in if remaining > 0
                          then dropLine remaining ws
                          else w: ws

lineLen :: Int
lineLen = 20

oneRing :: String
oneRing = "One Ring to rule them all, One Ring to find them, " ++
  "One Ring to bring them all and in the darkness bind them"

splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws = (getLine lineLen ws) : splitLines (dropLine lineLen ws)

fill :: String -> [Line]
fill = splitLines . splitWords

{- Ex 7.28 -}
joinLine :: Line -> String
joinLine []     = ""
joinLine (w:ws) = w ++ " " ++ joinLine ws

{- Ex 7.29 -}
joinLines :: [Line] -> String
joinLines []     = ""
joinLines (l:ls) = (joinLine l) ++ joinLines ls

{- Ex 7.32: returns the number of characters, words and lines -}
wc :: String -> (Int,Int,Int)
wc [] = (0, 0, 0)
wc cs = (length chars, length words, length lines)
  where lines = fill $ dropSpace cs
        words = concat [words | words <- lines]
        chars = concat [ch    | ch    <- words]
