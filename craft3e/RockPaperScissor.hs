{- #############################################################################
   Sample code from:
      Simon Thompson - Haskell: the Craft of Functional Programming, 2011
      ++++ Addison-Wesley ++++
      http://www.haskellcraft.com/craft3e/Home.html
############################################################################# -}

data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

score :: Move -> Move -> Int
score Rock Rock     = 0
score Rock Paper    = -1
score Rock Scissors = 1
score Paper Rock    = 1
