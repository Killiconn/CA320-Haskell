insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []