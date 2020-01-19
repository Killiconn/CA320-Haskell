delFirst :: Eq a => a -> [a] -> [a]


delFirst n [] = []
delFirst n (a:xs)
    |n == a = xs
    |otherwise = a : delFirst n xs