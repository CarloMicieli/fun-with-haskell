data Color = Black | Red deriving (Show, Read, Eq, Enum)

data Value = A | Two | Three | Four | Five | Six |
             Seven | Eight | Nine | Ten | J | Q | K
    deriving (Show, Read, Eq, Enum)

data Suit = Spades | Clubs | Diamonds | Hearts
    deriving (Show, Read, Eq, Enum)

data Card = Card { val  :: Value
                 , suit :: Suit
                 } deriving (Show, Read, Eq)

data Move = Draw Card | Discard Card
  deriving (Show, Read, Eq)

type Moves = [Move]

type Deck = [Card]

cards :: [Card]
cards = [ Card Nine Hearts
        , Card A Spades
        , Card Two Clubs
        , Card Three Clubs
        ]

deck :: Deck
deck = [Card v s | s <- [Spades .. Hearts] ,
                      v <- [A .. K]]

showMove :: Move -> String
showMove (Draw c)    = "drawing " ++ show c
showMove (Discard c) = "discarding " ++ show c

{-
    It takes a card and returns its color (spades and clubs are black,
    diamonds and hearts are red). Note: One case-expression is enough.
-}
cardColor :: Card -> Color
cardColor c = color (suit c)
    where color s | s == Spades || s == Clubs = Black
                  | otherwise                 = Red

{-
    It takes a card and returns its value (numbered cards have their
    number as the value, aces are 11, everything else is 10).
-}
cardValue :: Card -> Int
cardValue c = value (val c)
    where value v | v == A                     = 11
                  | v == J || v == Q || v == K = 10
                  | otherwise  = fromEnum v + 1


deleteCard :: Card -> [Card] -> [Card]
deleteCard _ [] = []
deleteCard c' (c:cs) | c == c'   = cs
                     | otherwise = c : (deleteCard c' cs)

deleteCard' :: Card -> [Card] -> (Bool, [Card])
deleteCard' _ [] = (False, [])
deleteCard' c xs = remove False [] xs
    where remove found acc []     = (found, acc)
          remove _     acc (y:ys) = if c == y
                                    then (remove True (acc ++ ys) [])
                                    else (remove False (y:acc) ys)

{-
    It takes a list of cards 'cs', a card 'c', and an exception 'e'. It returns
    a list that has all the elements of cs except c. If c is in the list more
    than once, remove only the first one.
    If c is not in the list, raise the exception e.
-}
removeCard :: Card -> [Card] -> Either String [Card]
removeCard _ [] = Left  "Prelude.removeCard: card not found."
removeCard c xs = remove False [] xs
    where remove found acc []     = if found
                                    then Right acc
                                    else Left "Prelude.removeCard: card not found."
          remove _     acc (y:ys) = if c == y
                                    then (remove True (acc ++ ys) [])
                                    else (remove False (y:acc) ys)

{-
    It takes a list of cards and returns true if all the cards in the
    list are the same color.
-}
allSameColor :: [Card] -> Bool
allSameColor xs = (allColor Black) || (allColor Red)
  where allColor color = all (sameColor color) xs
        sameColor color card = (cardColor card) == color

{-
    It takes a list of cards and returns the sum of their values
-}
sumCards :: [Card] -> Int
sumCards cs = sum [cardValue v | v <- cs]

{-
    It takes a card list (the held-cards) and an int (the goal) and computes
    the score.
    Let sum be the sum of the values of the held-cards. If sum is greater
    than goal, the preliminary score is three times (sum − goal), else the
    preliminary score is (goal − sum). The score is the preliminary score
    unless all the held-cards are the same color, in which case the score is
    the preliminary score divided by 2
-}
score :: Int -> [Card] -> Int
score goal heldCards = let sum         = sumCards heldCards
                           prelimScore = if sum > goal
                                         then (sum - goal) * 3
                                         else (goal - sum)
                        in prelimScore `div` if allSameColor heldCards
                                             then 1
                                             else 2

{-
    It runs a game. It takes a card list (the card-list) a move list
    (what the player “does” at each point), and an int (the goal) and returns
    the score at the end of the game after processing (some or all of) the
    moves in the move list in order.
-}
officiate :: Int -> Deck -> Moves -> Either String Int
officiate goal cards ms = case applyMoves ms [] cards of
    Left x          -> Left x
    Right heldCards -> Right $ score goal heldCards

applyMoves :: Moves -> [Card] -> [Card] -> Either String [Card]
applyMoves []            held cards = Right held
applyMoves ((Draw _):_)  held []    = Right held
applyMoves ((Draw m):ms) held cards =
    case (removeCard m cards) of
        Left x         -> Left "IllegalMove: card not found (draw)"
        Right remCards -> applyMoves ms (m:held) remCards
applyMoves ((Discard m):ms) held cards =
    case (removeCard m held) of
        Left x         -> Left "IllegalMove: card not found (discard)"
        Right remHeld  -> applyMoves ms remHeld (m:cards)


cardsList :: [Card]
cardsList = [ Card Two Spades
            , Card Four Hearts
            , Card Seven Clubs
            , Card A Hearts
            , Card K Spades
            ]

moves :: Moves
moves = [ Draw (Card Two Spades)
        , Draw (Card A Hearts)
        , Discard (Card Two Spades)
        ]

errMoves :: Moves
errMoves = [ Draw (Card Two Spades)
           , Draw (Card A Hearts)
           , Discard (Card Two Spades)
           , Discard (Card Four Spades)
           ]
