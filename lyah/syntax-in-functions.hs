{- #############################################################################
   Sample code from:
      Miran Lipovača - Learn You a Haskell for Great Good!, 2011
      ++++ no starch press ++++
      http://www.nostarch.com/lyah.htm
############################################################################# -}

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck. You pick " ++ show x

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n | n > 0     = n * factorial (n - 1)
            | otherwise = error "Prelude.factorial: undefined for n < 0"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1,y1) (x2,y2) = (x1+x2, y1+y2)

sumPairs :: [(Int,Int)] -> [Int]
sumPairs xs = [x + y | (x,y) <- xs]

head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:xs) = Just x

tail' :: [a] -> Either String [a]
tail' []     = Left "Prelude.tail': empty list"
tail' (x:xs) = Right xs

init' :: [a] -> Either String [a]
init' []  = Left "Prelude.init': empty list"
init' [x] = Right []
init' xs  = Right (remLast xs)
  where remLast [x]    = []
        remLast (x:xs) = x : remLast xs

last' :: [a] -> Maybe a
last' []     = Nothing
last' [x]    = Just x
last' (x:xs) = last' xs

tell :: (Show a) => [a] -> String
tell []      = "List is empty"
tell [x]     = "List has only 1 element: " ++ show x
tell (x:y:_) = "List begins with " ++ show x ++ ", " ++ show y

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

capital :: String -> String
capital "" = "String is empty"
capital str@(c:cs) | isLower c = "No capital, sorry"
                   | isUpper c = "Capital of " ++ str ++ " is " ++ show c
      where isUpper ch = ch `elem` ['A'..'Z']
            isLower ch = ch `elem` ['a'..'z']

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell _ 0 = error "Prelude.bmiTell: height cannot be 0!"
bmiTell 0 _ = error "Prelude.bmiTell: weight cannot be 0!"
bmiTell w h | bmi <= 18.5 = "You're underwight, you emo!"
            | bmi <= 25.0 = "You're supposedly normal"
            | bmi <= 30.0 = "You're fat"
            | otherwise   = "You're a whale!"
    where bmi = w / h ^ 2

max' :: (Ord a) => a -> a -> a
max' a b  | a > b     = a
          | otherwise = b

initials :: String -> String
initials name = [f] ++ ". " ++ [l] ++ "."
  where [firstname, lastname] = words name
        (f:_) = firstname
        (l:_) = lastname
