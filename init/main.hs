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
  | a < 10 = "hello"
  | (a > 10 && a < 20) = "world"
  |  otherwise = "hello world"