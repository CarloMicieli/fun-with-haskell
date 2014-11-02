data Pattern = Wildcard   | Variable String   | UnitP |
               ConstP Int | TripleP [Pattern] | ConstructorP (String,Pattern)
             deriving (Show, Read, Eq)

data Val = Const Int | Unit | Tuple [Val] | Constructor (String, Val)
             deriving (Show, Read, Eq)
