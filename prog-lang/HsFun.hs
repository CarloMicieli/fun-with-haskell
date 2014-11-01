import Data.List

{-
    It takes a list of strings and an int n and returns the n th element of the
    list where the head of the list is 1st.
-}
getNth :: Int -> [String] -> Maybe String
getNth 0 _      = Nothing
getNth _ []     = Nothing
getNth 1 (x:_)  = Just x
getNth i (x:xs) = getNth (i - 1) xs

{-
    It takes an int called sum, which you can assume is positive, and an
    int list, which you can assume contains all positive numbers, and
    returns an int.
    You should return an int n such that the first n elements of the list
    add to less than sum, but the first n + 1 elements of the list add to sum
    or more. Assume the entire list sums to more than the passed in
    value; it is okay for an exception to occur if this is not the case.
-}
numbersBeforeSum :: Int -> [Int] -> Int
numbersBeforeSum sum xs = length $ takeWhile (< sum) (scanl1 (+) xs)

{-
    It takes a string and a string list. Return 'Nothing' if the string is not
    in the list, else return 'Just' lst where lst is identical to the argument
    list except the string is not in it. You may assume the string is in the
    list at most once. Use same_string, provided to you,
    to compare strings
-}
allExcept :: String -> [String] -> Maybe [String]
allExcept _ [] = Nothing
allExcept s xs = filterFound False [] xs
    where filterFound False _ []      = Nothing
          filterFound found zs (y:ys) = if s == y
                                        then Just (zs ++ ys)
                                        else (filterFound False (y:zs) ys)

type Substitutions = [[String]]
subs :: Substitutions
subs = [["Fred","Fredrick"], ["Elizabeth","Betty"], ["Freddie","Fred","F"]]

{-
    It takes a string list list (a list of list of strings, the substitutions)
    and a string s and returns a string list. The result has all the strings
    that are in some list in substitutions that also has s, but s itself
    should not be in the result.
-}
getSubstitutions :: String -> Substitutions -> [String]
getSubstitutions str xs = concat $ map (delete str) $ filter (elem str) xs

data FullName = FullName { firstName :: String
                         , middle    :: String
                         , lastName  :: String
                         } deriving (Show, Eq)

fred :: FullName
fred = FullName { firstName = "Fred"
                , middle = "W"
                , lastName = "Smith" }

{-
    It takes a string list list of substitutions and a full name of type
    {first:string,middle:string,last:string} and returns a list of full
    names (type {first:string,middle:string,last:string} list)
-}
similarNames :: FullName -> Substitutions -> [FullName]
similarNames fn@(FullName n m l) subs = let names = getSubstitutions n subs
                                        in fn : [ FullName x m l | x <- names ]
