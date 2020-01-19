elemSorted :: Ord a => a -> [a] -> Bool

elemSorted n [] = False
elemSorted n (x:xs)
   |x > n = False
   |x == n = True
   |otherwise = elemSorted n xs