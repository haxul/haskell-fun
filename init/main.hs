import Data.Char
import Data.List
import qualified Data.Map as Map

sumUs x y =  y + x
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x < 100 then x else doubleMe x 

length' xs = sum [ 1 | _ <- xs]
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

sumThreeOne = sumThree 1

removeUppercase :: [Char] -> [Char]
removeUppercase xs = [x | x <- xs, x `elem` '\n' : ['a' .. 'z']]

lucky :: Int -> String
lucky 7 = "you are luckier"
lucky x = "sorry man"

head' :: [a] -> a
head' [] = error "wtf empty list"
head' (x: xs) = x

firstLetter :: String -> String
firstLetter all@(x : xs) = "all = " ++ all ++ ", first letter is" ++ [x]

tell :: (Num a) => (Ord a) => a -> String
tell a 
  | a < normal = "hello"
  | (a > normal && a < big) = "world"
  |  otherwise = "hello world"
  where 
    normal = 10
    big = 20

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

testmaximium' :: (Ord a) => [a] -> a
testmaximium' [] = error "sorry"
testmaximium' [x] = x
testmaximium' (x:xs) = max x (testmaximium' xs)

product' :: (Ord a) => (Num a) => [a] -> a
product' [] = error "error"
product' [x] = x
product' (xs) = x * (product' xxs)
  where x = head xs  
        xxs = tail xs

replicate' :: a -> Int-> [a]
replicate' x 0 = []
replicate' x y = x : replicate' x (y-1)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' x (y:ys) = y : take (x-1) ys

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = [x] ++ repeat' x

zip' :: [a]->[a]->[(a,a)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = if y == x then True else elem x ys

devideBy10 :: Double -> Double
devideBy10 = (/10)

applyTwice :: (a-> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a-> a-> a) -> [a] -> [a] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

map' :: (a -> a) -> [a] -> [a]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x :xs) = if f x == True then x : filter' f xs else filter' f xs  

largeDivBy3829 :: [Integer]
largeDivBy3829 =  [ x | x <- filter p [100000, 99999..0]]
  where p x = x `mod` 3829 == 0


takeWhile':: (a->Bool)->[a]->[a]
takeWhile' _ [] = []
takeWhile' p (x : xs) 
  | p x == True = x : takeWhile' p xs
  | otherwise = takeWhile' p []

chain :: Integer -> [Integer]
chain 1 = [1]
chain n  
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15


mapTest :: (a -> b) -> [a] -> [b]
mapTest f xs = foldl (\acc x -> f x : acc ) [] xs 


firstTo40 :: Maybe Int
firstTo40 = find (\x -> x ==40 ) [1..5]

phoneBook =
  [("betty", "555-2938")
  ,("bonnie", "452-2928")
  ,("patsy", "493-2928")
  ,("lucille", "205-2928")
  ,("wendy", "939-8282")
  ,("penny", "853-2492")
  ]

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs) 
    | k == key = Just v
    | otherwise = findKey' key xs


fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Oops, you goofed up, fool."
result = (++) "hello " $ fromJust $ findKey' "betty" phoneBook 

--functionalComposition = (\xs -> negate (sum (tail xs))) 
--functionalComposition =  negate . sum . tail   

