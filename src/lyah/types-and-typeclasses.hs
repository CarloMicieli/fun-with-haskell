{- #############################################################################
   Sample code from:
      Miran Lipovača - Learn You a Haskell for Great Good!, 2011
      ++++ no starch press ++++
      http://www.nostarch.com/lyah.htm
############################################################################# -}

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase s = [ch | ch <- s , ch `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree m n p = m + n + p

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r
