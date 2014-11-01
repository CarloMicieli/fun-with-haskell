{- #############################################################################
   Sample code from:
      Miran Lipovača - Learn You a Haskell for Great Good!, 2011
      ++++ no starch press ++++
      http://www.nostarch.com/lyah.htm
############################################################################# -}

sumSucc :: Int -> Int -> Int
sumSucc m n = (succ m) + (succ n)

doubleMe :: (Num a) => a -> a
doubleMe n = n * 2

doubleUs :: (Num a) => a -> a -> a
doubleUs m n = doubleMe m + doubleMe n

doubleSmallNums :: (Num a, Ord a) => a -> a
doubleSmallNums n = if n > 100 then n
                    else            doubleMe n


numbers :: [Int]
numbers = [4,8,15,16,23,42]

helloWorld :: [Char]
helloWorld = "hello" ++ " " ++ "world"

elemAt :: Int -> [a] -> a
elemAt _ [] = error "Prelude.elemAt: empty list"
elemAt i xs = xs !! i

evenNumbersAt :: Int -> [Int]
evenNumbersAt n = [x | x <- [0..n] , even x]

generate :: (Int -> Bool) -> Int -> [Int]
generate p n = [x | x <- [0..n] , p x]

boomBangs :: (Integral a) => [a] -> [String]
boomBangs xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs , odd x]

rightTriangles :: Int -> [(Int,Int,Int)]
rightTriangles n = [ (a,b,c) |
  c <- [1..n] ,
  b <- [1..c] ,
  a <- [1..b] , a ^ 2 + b ^ 2 == c ^ 2 ]
