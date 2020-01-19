ifelem :: Integer -> [Integer] -> Bool

ifelem n [] = False
ifelem n (x:xs)
   |n == x = True
   |otherwise = ifelem n xs
