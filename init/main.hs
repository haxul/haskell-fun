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






