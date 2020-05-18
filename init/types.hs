
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


