data Month = January | February | March     |
             April   | May      | June      |
             July    | August   | September |
             October | November | December
             deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Date = Date { year  :: Int
                 , month :: Month
                 , day   :: Int
                 } deriving (Show, Read, Eq)

instance Ord Date where
    d1 < d2  = isOlder d1 d2
    d1 <= d2 = d1 == d2 || isOlder d1 d2

halloween :: Date
halloween = Date 2014 October 31

programmerDay :: Date
programmerDay = Date 2014 September 12

dates :: [Date]
dates = [ Date 2013 October 12
        , Date 1981 September 9
        , Date 1976 July 4
        , Date 1991 July 18
        , Date 1996 August 6
        ]

toDate :: (Int,Int,Int) -> Date
toDate (y,m,d) = Date (toYear y) (toMonth m) (toDay d)
    where toYear yy | yy < 1970 || yy > 2999 = error "Prelude.toDate: invalid year"
                    | otherwise              = yy
          toMonth mm | mm < 0 || mm > 12     = error "Prelude.toDate: invalid month"
                     | otherwise             = getMonth mm
          toDay   dd | dd < 0 || dd > (days $ getMonth m)
                                             = error "Prelude.toDate: invalid day"
                     | otherwise             = dd
          getMonth m = toEnum (m - 1)

monthDays :: [(Int, Month)]
monthDays = daysMonth (days January) [January .. December]
    where daysMonth _  []     = []
          daysMonth dd (m:ms) = (dd, m) : daysMonth (days m + dd) ms

-- |Returns the number of days for this `Month`
days :: Month -> Int
days February = 28
days m | m == April || m == June || m == September || m == November = 30
       | otherwise = 31

yearDays :: Date -> Int
yearDays (Date _ m d) = d + sum [days m | m <- [January .. (pred m)]]

fromMonth :: Month -> Int
fromMonth m = 1 + fromEnum m

showDate :: Date -> String
showDate (Date y m d) = show m ++ " " ++ show d ++ ", " ++ show y

{-
    it takes two 'Date's and evaluates to 'True' or 'False'. It evaluates to
    'True' if the first argument is a 'Date' that comes before the second
    argument.
    (If the two 'Date's are the same, the result is 'False')
-}
isOlder :: Date -> Date -> Bool
isOlder d1 d2 | d1 == d2 = False
isOlder (Date y1 m1 d1) (Date y2 m2 d2)
    | y1 < y2                         = True
    | y1 == y2 && m1 < m2             = True
    | y1 == y2 && m1 == m2 && d1 < d2 = True
    | otherwise                       = False

{-
    It takes a list of 'Date's and a 'Month' and returns
    how many 'Date's in the list are in the given 'Month'.
-}
howManyInMonth :: Month -> [Date] -> Int
howManyInMonth m ds = length $ datesInMonth m ds

{-
    It takes a list of dates and a list of months and returns the
    number of dates in the list of dates that are in any of the months
    in the list of months.
-}
howManyInMonths :: [Month] -> [Date] -> Int
howManyInMonths ms ds = length $ datesInMonths ms ds

{-
    It takes a list of dates and a month and returns a list holding the dates
    from the argument list of dates that are in the month.
    The returned list should contain dates in the order they
    were originally given.
-}
datesInMonth :: Month -> [Date] -> [Date]
datesInMonth m ds = [d | d <- ds , m == (month d)]

{-
    It takes a list of dates and a list of months and returns a list
    holding the dates from the argument list of dates that are in any of the
    months in the list of months
-}
datesInMonths :: [Month] -> [Date] -> [Date]
datesInMonths []     ds = []
datesInMonths (m:ms) ds = (datesInMonth m ds) ++ (datesInMonths ms ds)

{-
    It takes a day of year (i.e., an int between 1 and 365) and returns
    what month that day is in (1 for January, 2 for February, etc.).
-}
whatMonth :: Int -> Month
whatMonth d | d < 1 || d > 365 = error "Prelude.whatMonth: invalid day of year"
whatMonth day = snd $ head (dropWhile (\(d, m) -> d < day) monthDays)

{-
    It takes a date and return the next day in the current year.
-}
next :: Date -> Date
next (Date _ December 31) = error "Prelude.next: reached the end of year"
next (Date y m d) = if d + 1 <= days m then (Date y m (d + 1))
                    else                    (Date y (succ m) 1)

{-
    It takes two days of the year day1 and day2 and returns an int list
    [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of
    day1+1, ..., and mn is the month of day day2
-}
monthRange :: Date -> Date -> [Int]
monthRange (Date y1 _ _) (Date y2 _ _) | y1 /= y2 =
    error "Prelude.monthRange: dates must have the same year"
monthRange day1 day2 = (mRange day1 day2)
    where mRange d1 d2 | d1 >= d2  = []
                       | otherwise = (getMonth d1) : (mRange (next d1) d2)
          getMonth d = fromMonth $ month d

{-
    It takes a list of dates and evaluates to a Maybe Date.
    It evaluates to Nothing if the list has no dates and Just d if the date d
    is the oldest date in the list.
-}
oldest :: [Date] -> Maybe Date
oldest [] = Nothing
oldest ds = Just $ foldl1 (old) ds
    where old d1 d2 = if d1 <= d2 then d1 else d2
