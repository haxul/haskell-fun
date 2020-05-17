sumUs x y =  y + x
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x < 100 then x else doubleMe x 
length' xs = sum [ 1 | _ <- xs]
removeUppercase xs = [x | x <- xs, x `elem` '\n' : ['a' .. 'z']]