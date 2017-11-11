{- #############################################################################
   Sample code from:
      Simon Thompson - Haskell: the Craft of Functional Programming, 2011
      ++++ Addison-Wesley ++++
      http://www.haskellcraft.com/craft3e/Home.html
############################################################################# -}

module Craft3e.Chapter03 where

-- |The AND operator
andAlso :: Bool -> Bool -> Bool
andAlso e1 e2 = if e1 then e2 else False

prop1_andAlso :: Bool -> Bool -> Bool
prop1_andAlso e1 e2 = e1 `andAlso` e2 == (e1 && e2)

test1a_andAlso = TestCase (assertEqual "True `andAlso` True: " True (True `andAlso` True))
test1b_andAlso = TestCase (assertEqual "False `andAlso` True: " False (False `andAlso` True))

-- |The OR operator
orElse :: Bool -> Bool -> Bool
orElse e1 e2 = if e1 then True else e2

prop2_orElse :: Bool -> Bool -> Bool
prop2_orElse e1 e2 = e1 `orElse` e2 == (e1 || e2)

test2a_orElse = TestCase (assertEqual "True `orElse` False: " True (True `orElse` False))
test2b_orElse = TestCase (assertEqual "False `orElse` False: " False (False `orElse` False))

-- |The NOT operator
not' :: Bool -> Bool
not' True  = False
not' False = True

prop3_not :: Bool -> Bool
prop3_not e = not' e == not e

test3a_not = TestCase (assertEqual "not' False: " True (not' False))
test3b_not = TestCase (assertEqual "not' True: " False (not' True))

-- |The ExOR operator
exOr :: Bool -> Bool -> Bool
exOr e1 e2 = if e1 then not' e2
             else       e2

prop4_exOr :: Bool -> Bool -> Bool
prop4_exOr e1 e2 = e1 `exOr` e2 == ((e1 && not e2) || (not e1 && e2))

test4_exOr = TestCase (assertEqual "exOr False False: " False (False `exOr` False))
tests = TestList [test1a_andAlso, test1b_andAlso,
                  test2a_orElse, test2b_orElse,
                  test3a_not, test3b_not,
                  test4_exOr]