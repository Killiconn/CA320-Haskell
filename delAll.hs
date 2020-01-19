delAll :: Eq a => a -> [a] -> [a]

delAll n [] = []
delAll n (a:xs)
    |n == a = delAll n xs
    |otherwise = a : delAll n xs