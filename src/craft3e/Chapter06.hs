{- #############################################################################
   Sample code from:
      Simon Thompson - Haskell: the Craft of Functional Programming, 2011
      ++++ Addison-Wesley ++++
      http://www.haskellcraft.com/craft3e/Home.html
############################################################################# -}

module Craft3e.Chapter06 where
    
import Prelude hiding (lookup)

type Name    = String
type Price   = Int
type BarCode = Int

type Database = [ (BarCode,Name,Price) ]

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers",        121)
            , (5643, "Nappies"     ,       1010)
            , (3814, "Orange Jelly",         56)
            , (1111, "Hula Hoops",           21)
            , (1112, "Hula Hoops (Giant)",  133)
            , (1234, "Dry Sherry, 1lt",     540)
            ]

type TillType = [BarCode]
type BillType = [ (Name,Price) ]

lineLength :: Int
lineLength = 30

barcodes :: TillType
barcodes = [1234, 4719, 3814, 1112, 1113, 1234]

bill :: BillType
bill = [ ("Dry Sherry, 1lt",     540)
        , ("Nappies"     ,       1010)
        , ("Orange Jelly",         56)
        , ("Dry Sherry, 1lt",     540) ]

{- Ex 6.39 -}
formatPence :: Price -> String
formatPence p = let pounds = p `div` 100
                    pence  = p `mod` 100
                    in fmtPounds pounds ++ "." ++ fmtPence pence
            where fmtPounds x = show x
                    fmtPence  x = if x < 10 then '0' : show x
                                else                 show x

{- Ex 6.40 -}
formatLine :: (Name,Price) -> String
formatLine (name, price) = let priceStr = formatPence price
                                len = lineLength - length name - length priceStr
                            in name ++ (replicate len '.') ++ priceStr ++ "\n"
{- Ex 6.41 -}
formatLines :: [(Name,Price)] -> String
formatLines lines = concat $ map formatLine lines

{- Ex 6.42 -}
makeTotal :: BillType -> Price
makeTotal bill = let disc = makeDiscount bill
                    in sum [price | (_,price) <- bill] - disc

{- Ex 6.43 -}
formatTotal :: Price -> String
formatTotal tot = "\n" ++ formatLine ("Total", tot)

{- Ex 6.44 -}
storeHead :: String
storeHead = "Haskell Stores"

formatBill :: BillType -> String
formatBill bill = let len = (lineLength - length storeHead) `div` 2
                        header = (replicate len ' ') ++ storeHead ++ "\n\n"
                        discount = formatDiscount $ makeDiscount bill
                        footer = formatTotal $ makeTotal bill
                    in header ++ formatLines bill ++ discount ++ footer

{- Ex 6.45 -}
look :: Database -> BarCode -> (Name,Price)
look dBase bc = let results = [(n,p) | (b,n,p) <- dBase , b == bc]
                    in if null results then ("Unknown item", 0)
                    else                 head results

{- Ex 6.46 -}
lookup :: BarCode -> (Name,Price)
lookup bc = look codeIndex bc

{- Ex 6.47 -}
makeBill :: TillType -> BillType
makeBill barCodes = map lookup barCodes

produceBill :: TillType -> String
produceBill = formatBill . makeBill

printBill :: IO ()
printBill = putStr $ produceBill barcodes

{- Ex 6.48 -}
makeDiscount :: BillType -> Price
makeDiscount bill = if applyDiscount then 100 else 0
    where applyDiscount = buyOfSherryCount > 1
            buyOfSherryCount = sum [1 | (n,_) <- bill , n == "Dry Sherry, 1lt"]

formatDiscount :: Price -> String
formatDiscount 0    = ""
formatDiscount disc = "\n" ++ formatLine ("Discount", disc)

{- Ex 6.49 -}
addBarCode :: Database -> BarCode -> (Name,Price) -> Database
addBarCode []              barCode (name,price) = [(barCode,name,price)]
addBarCode (x@(bc,_,_):xs) barCode (name,price) =
    if bc == barCode then addBarCode xs barCode (name,price)
    else x : addBarCode xs barCode (name,price)


{- Ex 6.53 -}
data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Read, Eq, Enum)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
                Jack | Queen | King | Ace
    deriving (Show, Read, Eq, Enum)

data Player = East | South | West | North
    deriving (Read, Eq, Enum)

instance Show Player where
    show East  = "E"
    show West  = "W"
    show North = "N"
    show South = "S"

data Trick = Trick Player Suit
    deriving (Eq)

suit :: Trick -> Suit
suit (Trick _ s) = s

lead :: Trick -> Player
lead (Trick pl _) = pl