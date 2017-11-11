import Data.Char

quote :: String
quote = "Everyone knows that debugging is twice as hard as writing " ++
  "a program in the first place. So if you're as clever as you can " ++
  "be when you write it, how will you ever debug it?"

strings :: [String]
strings = words quote

longest :: String -> String -> String
longest m n = if length m >= length n
              then m
              else n

isEven :: Int -> [Maybe Int]
isEven n = if even n then [Just n] else [Nothing]

{-
  It takes a string list and returns a string list that has only the strings
  in the argument that start with an uppercase letter. Assume all strings have
  at least 1 character
-}
onlyCapitals :: [String] -> [String]
onlyCapitals xs = [x | x <- xs , startsWithUpper x]
  where startsWithUpper (c:cs) = if isUpper c then True else False

{-
  It takes a string list and returns the longest string in the list. If the
  list is empty, return "". In the case of a tie, return the string closest
  to the beginning of the list
-}
longestString :: [String] -> String
longestString [] = ""
longestString xs = foldl1 (longest) xs

longestString' :: [String] -> String
longestString' [] = ""
longestString' xs = foldl1 (longest) xs
  where longest m n = if length m > length n then m else n

longestString'' :: (Int -> Int -> Bool) -> [String] -> String
longestString'' _ [] = ""
longestString'' p xs = foldl1 (longest) xs
  where longest s longStr = if p (length s) (length longStr) then s else longStr

{-
  It takes a string list and returns the longest string in the list that
  begins with an uppercase letter, or "" if there are no such strings. Assume
  all strings have at least 1 character.
-}
longestCapitalized :: [String] -> String
longestCapitalized [] = ""
longestCapitalized xs = longestString $ onlyCapitals xs

{-
  The first argument should be applied to elements of the second argument in
  order until the first time it returns 'Just v' for some v and then v is
  the result of the call to firstAnswer.
  If the first argument returns 'Nothing' for all list elements, then
  firstAnswer should raise the error NoAnswer.
-}
firstAnswer :: (Eq b) => (a -> Maybe b) -> [a] -> b
firstAnswer f xs = case take 1 $ dropWhile (==Nothing) (map (f) xs) of
  []         -> error "Prelude.firstAnswer: NoAnswer"
  [Just ans] -> ans

{-
  The first argument should be applied to elements of the second argument. If
  it returns 'Nothing' for any element, then the result for 'allAnswers' is
  'Nothing'. Else the calls to the first argument will have produced
  'Just lst1', 'Just lst2', ... 'Just lstn' and the result of allAnswers is
  'Just lst' where 'lst' is 'lst1, lst2, ..., lstn' appended together.
-}
allAnswers :: (Eq b) => (a -> [Maybe b]) -> [a] -> [Maybe b]
allAnswers f xs = let answers = concat $ map (f) xs
                  in if all (/=Nothing) answers
                     then answers
                     else [Nothing]
