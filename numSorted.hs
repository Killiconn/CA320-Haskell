numSorted :: Ord a => a -> [a] -> Int

numSorted n [] = 0
numSorted n (a:xs)
    |n < a = 0
    |n == a = numSorted n xs + 1
    |otherwise = numSorted n xs