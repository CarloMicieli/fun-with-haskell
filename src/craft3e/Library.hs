{- #############################################################################
   Sample code from:
      Simon Thompson - Haskell: the Craft of Functional Programming, 2011
      ++++ Addison-Wesley ++++
      http://www.haskellcraft.com/craft3e/Home.html
############################################################################# -}

module Craft3e.Library where
    
import Data.List
import Test.QuickCheck

type Person = String
type Book   = String
type Loan   = (Person, Book)

type Database = [Loan]

exampleBase :: Database
exampleBase = [ ("Alice", "Tintin")
                , ("Anna" , "Little Women")
                , ("Alice", "Asterix")
                , ("Rory" , "Tintin")
                ]

hasBook :: Book -> Loan -> Bool
hasBook b' (p, b) = b == b'

-- Given a person returns the books she has borrowed, if any
books :: Database -> Person -> [Book]
books db p = [ book | (p', book) <- db , p == p' ]

-- Given a book returns the borrowers of the book, if any
borrowers :: Database -> Book -> [Person]
borrowers db b = map fst $ filter (hasBook b) db

-- Given a book checks whether it is borrowed
borrowed :: Database -> Book -> Bool
borrowed db b = any (hasBook b) db

-- Given a person returns the number of books that she has borrowed
numBorrowed :: Database -> Person -> Int
numBorrowed db p = sum [1 | (p',_) <- db , p == p']

makeLoan :: Database -> Loan -> Database
makeLoan db loan = loan : db

returnLoan :: Database -> Loan -> Database
returnLoan = flip delete

prop_db1 :: Database -> Person -> Book -> Bool
prop_db1 dBase pers bk =
    elem bk loanedAfterLoan == True
            where afterLoan       = makeLoan dBase (pers, bk)
                loanedAfterLoan = books afterLoan pers

prop_db2 :: Database -> Person -> Book -> Bool
prop_db2 dBase pers bk =
    elem bk loanedAfterReturn == False
            where afterReturn       = returnLoan dBase (pers, bk)
                loanedAfterReturn = books afterReturn pers