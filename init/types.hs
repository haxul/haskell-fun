import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area:: Shape -> Float
area (Circle _ r) = 3.14 * r ^ 2
area (Rectangle (Point a b) (Point c d)) = (abs $ a - b) * (abs $ c - d)

response = area $ Rectangle  (Point 1 1) (Point 2 2) 
response2 = area $ Circle (Point 2 2) 21  

data Person = Person String String Int  deriving (Show)
guy = Person "sergei" "starodubov" 22 

getFirstName:: Person -> String
getFirstName (Person name _ _ ) = name

getLastName :: Person -> String
getLastName (Person _ lastname _ ) = lastname

getAge :: Person -> Int
getAge (Person _ _ age) = age


setAge :: Person -> Int -> Person
setAge (Person name lastname age ) newAge = Person name lastname newAge

data Human = Human {
  name::String,
  lastname::String,
  age :: Int
} deriving (Show)

data Car a b c = Car {
  company :: a,
  model ::b,
  year :: c

} deriving (Show)


stang = Car {company = "ford", model = "mustang", year = 1988 }
stang2 = Car {company = "ford", model = "mustang", year = "1988" }


data Coder = Coder {
  nick :: String,
  position :: String,
  old :: Int
} deriving (Show, Eq, Read)

coder = Coder {nick = "haxul", position="backend", old = 34}

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

number :: PhoneNumber
number = "12312313123"
polyName:: Name
polyName = "John"

phoneBook :: PhoneBook
phoneBook = [(polyName, number)]

type SomeData v = (Int , v)
someData :: SomeData String
someData = (1, "String")

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap
lockers = Map.fromList
  [
   (100,(Taken, "ZD39I"))
  ,(101,(Free, "JAH3I"))
  ,(103,(Free, "IQSA9"))
  ,(105,(Free, "QOTSA"))
  ,(109,(Taken, "893JJ"))
  ,(110,(Taken, "99292"))
  ]

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =  case Map.lookup lockerNumber map of 
  Nothing -> Left "code doesnt exist"
  Just (status, code) -> if status /= Taken then Right code else Left $ code ++ " is taken"

