{- #############################################################################
   Sample code from:
      Simon Thompson - Haskell: the Craft of Functional Programming, 2011
      ++++ Addison-Wesley ++++
      http://www.haskellcraft.com/craft3e/Home.html
############################################################################# -}

import Test.QuickCheck hiding (Result)

data Move = Rock | Paper | Scissors
  deriving (Eq, Show, Read, Enum)

instance Arbitrary Move where
  arbitrary = elements [Rock, Paper, Scissors]

data Result = Win | Draw | Lose
  deriving (Eq, Show, Read, Enum)

instance Arbitrary Result where
  arbitrary = elements [Win, Draw, Lose]

score :: Move -> Move -> Int
score Rock Rock     = 0
score Rock Paper    = -1
score Rock Scissors = 1
score Paper Rock    = 1

beat :: Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock      = Scissors
lose Paper     = Rock
lose _         = Paper

moves_prop1 :: Move -> Bool
moves_prop1 m = (beat . lose) m == m

outcome :: Move -> Move -> Result
outcome m1 m2 | m1 == m2 = Draw
outcome m1 m2
  | m1 == beat m2        = Win
  | otherwise            = Lose

outcome_prop1 :: Move -> Move -> Bool
outcome_prop1 m n = let o1 = outcome m n
                        o2 = outcome n m
                     in compareOutcome o1 o2
                  where compareOutcome Draw Draw = True
                        compareOutcome Win  Lose = True
                        compareOutcome Lose Win  = True
                        compareOutcome _    _    = False
