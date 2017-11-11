{- #############################################################################
   Sample code from:
      Simon Thompson - Haskell: the Craft of Functional Programming, 2011
      ++++ Addison-Wesley ++++
      http://www.haskellcraft.com/craft3e/Home.html
############################################################################# -}

module Craft3e.HsGame where

import Data.List hiding (cycle)
import Test.QuickCheck hiding (Result)
import Prelude hiding (cycle)

data Move = Rock | Paper | Scissors
    deriving (Eq, Ord, Show, Read, Enum)

instance Arbitrary Move where
    arbitrary = elements [Rock, Paper, Scissors]

data Result = Win | Draw | Lose
    deriving (Eq, Ord, Show, Read, Enum)

instance Arbitrary Result where
    arbitrary = elements [Win, Draw, Lose]

type Tournament = ([Move], [Move])

game :: Tournament
game = ([Rock,Rock,Paper], [Scissors,Paper,Rock])

moves :: [Move]
moves = [Rock,Rock,Paper, Scissors,Paper,Rock]

initial :: Move
initial = Rock

type Strategy = [Move] -> Move
rock, paper, scissors :: Strategy

rock _     = Rock
paper _    = Paper
scissors _ = Scissors

cycleMoves :: Strategy
cycleMoves moves = case (length moves) `rem` 3 of
    0 -> Rock
    1 -> Paper
    2 -> Scissors

echo :: Strategy
echo (m:_) = m
echo []    = initial

beatLast :: Strategy
beatLast  []   = initial
beatLast (m:_) = beat m

loseLast :: Strategy
loseLast  []   = initial
loseLast (m:_) = lose m

byFrequency :: Strategy
byFrequency [] = initial
byFrequency ms = let movesFreq = sort $ map (\ ms -> (length ms, head ms)) $ group $ sort ms
                in snd $ head $ movesFreq

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

tournamentOutcome :: Tournament -> (Result, Result)
tournamentOutcome ([], []) = (Draw, Draw)
tournamentOutcome t  = let (ma, mb) = t
                            outcomes = filter (/= Draw) $ map (toOutcome) $ zip ma mb
                            results  = map (\r -> (head r, length r)) $ group $ sort outcomes
                        in toResult results
                        where toOutcome (m, n) = outcome m n
                            toResult []                 = (Draw, Draw)
                            toResult [(Win,w),(Lose,l)] | w > l     = (Win, Lose)
                                                        | w < l     = (Lose, Win)
                                                        | otherwise = (Draw, Draw)
                            toResult [(Win,_)]          = (Win, Lose)
                            toResult [(Lose,_)]         = (Lose, Win)