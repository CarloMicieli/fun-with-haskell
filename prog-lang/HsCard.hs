data Color = Black | Red deriving (Show, Read, Eq, Enum)

data Value = A | Two | Three | Four | Five | Six |
             Seven | Eight | Nine | J | Q | K
    deriving (Show, Read, Eq, Enum)

data Suit = Spades | Clubs | Diamonds | Hearts
    deriving (Show, Read, Eq, Enum)

data Card = Card { val  :: Value
                 , suit :: Suit
                 } deriving (Show, Read, Eq)

type Cards = [Card]

deck :: Cards
deck = [ Card Nine Hearts
       , Card A Spades
       , Card Two Clubs
       , Card Three Clubs
       ]

{-
    takes a card and returns its color (spades and clubs are black,
    diamonds and hearts are red). Note: One case-expression is enough.
-}
cardColor :: Card -> Color
cardColor c = color (suit c)
    where color s | s == Spades || s == Clubs = Black
                  | otherwise                 = Red

{-
    takes a card and returns its value (numbered cards have their
    number as the value, aces are 11, everything else is 10).
-}
cardValue :: Card -> Int
cardValue c = value (val c)
    where value v | v == A     = 11
                  | v == Two   = 2
                  | v == Three = 3
                  | v == Four  = 4
                  | v == Five  = 5
                  | v == Six   = 6
                  | v == Seven = 7
                  | v == Eight = 8
                  | v == Nine  = 9
                  | otherwise  = 10

{-
    takes a list of cards 'cs', a card 'c', and an exception 'e'. It returns a
    list that has all the elements of cs except c. If c is in the list more
    than once, remove only the first one.
    If c is not in the list, raise the exception e.
-}
removeCard :: Cards -> Card -> Either String Cards
removeCard cs c = fromList (remove False cs)
    where fromList [] = Left  "Prelude.removeCard: not found."
          fromList xs = Right xs
          remove False []      = []
          remove True  xs      = xs
          remove _     (c':cs) = if c == c'
                                 then (remove True cs)
                                 else c' : (remove False cs)
