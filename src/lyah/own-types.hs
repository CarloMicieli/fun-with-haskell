{- #############################################################################
   Sample code from:
      Miran Lipovača - Learn You a Haskell for Great Good!, 2011
      ++++ no starch press ++++
      http://www.nostarch.com/lyah.htm
############################################################################# -}

data Point = Point Float Float
  deriving (Show, Read, Eq)

data Shape = Circle Point Float | Rectangle Point Point
  deriving (Show, Read)

origin :: Point
origin = Point 0.0 0.0

-- |Creates a new 'Circle' at the origin.
circle :: Float -> Shape
circle = Circle origin

-- |Creates a new 'Rectangle' at the origin with the provided height and width
rect :: Float -> Float -> Shape
rect height width = Rectangle origin (Point height width)

getX :: Point -> Float
getX (Point x _) = x

getY :: Point -> Float
getY (Point _ y) = y

height :: Shape -> Float
height (Rectangle (Point _ y1) (Point _ y2)) = abs $ y2 - y1
height _ = error "Prelude.height: works only with Rectangle"

width :: Shape -> Float
width (Rectangle (Point x1 _) (Point x2 _)) = abs $ x2 - x1
width _ = error "Prelude.width: works only with Rectangle"

area :: Shape -> Float
area (Circle _ r)         = pi * r ^ 2
area rect@(Rectangle _ _) = (height rect) * (width rect)

perimeter :: Shape -> Float
perimeter (Circle _ r)    = 2 * pi * r
perimeter rect@(Rectangle _ _) = 2 * (height rect + width rect)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b                  =
  Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


data Person = Person { firstName   :: String
                     , lastName    :: String
                     , age         :: Int
                     , phoneNumber :: String
                     } deriving (Show, Read, Eq)

mikeD = Person { firstName = "Micheal"
               , lastName  = "Diamond"
               , age       = 43
               , phoneNumber = "555-1234"
               }
adRock = Person { firstName = "Adam"
                , lastName = "Horovitz"
                , age      = 41
                , phoneNumber = "555-6748"
                }

data Car = Car { company :: String, model :: String, year :: Int }
  deriving (Show)

mustang :: Car
mustang = Car { company = "Ford" , model = "Mustang" , year = 1967 }

camaro :: Car
camaro = Car "Chevrolet" "Camaro" 2010

tellCar :: Car -> String
tellCar (Car{ company = c, model = m, year = y }) =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

first :: Car -> Car -> Car
first c1@(Car _ _ year1) c2@(Car _ _ year2) | year1 <= year2 = c1
                                            | otherwise      = c2


data Vector a = Vector a a a deriving (Show)

v1 :: Vector Int
v1 = Vector 1 2 3

v2 :: Vector Int
v2 = Vector 4 5 6

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2) =
  Vector (x1+x2) (y1+y2) (z1+z2)

vectMul :: (Num t) => Vector t -> t -> Vector t
(Vector x y z) `vectMul` m = Vector (m*x) (m*y) (m*z)

scalarMul :: (Num t) => Vector t -> Vector t -> t
(Vector x1 y1 z1) `scalarMul` (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

firstDay :: Day
firstDay = minBound :: Day

lastDay  :: Day
lastDay = maxBound :: Day

nextDay :: Day -> Day
nextDay Sunday = Monday
nextDay d = succ d

days :: [Day]
days = [minBound .. maxBound]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name , PhoneNumber)]
phoneBook :: PhoneBook
phoneBook =
  [("betty"  , "555-2938")
  ,("bonnie" , "555-2928")
  ,("patsy"  , "555-4931")
  ,("lucille", "555-0205")
  ,("wendy"  , "555-8282")
  ]

searchPhoneNumber :: Name -> PhoneBook -> Maybe PhoneNumber
searchPhoneNumber _ []     = Nothing
searchPhoneNumber n ((n',pn):es) = if n == n' then Just pn
                                   else            (searchPhoneNumber n es)
