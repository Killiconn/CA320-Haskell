nubSorted :: Eq a => [a] -> [a]

nubSorted [] = []
nubSorted [x] = [x]
nubSorted (x:xs)
   |x == head xs = nubSorted (xs)
   |otherwise = x : nubSorted (xs)