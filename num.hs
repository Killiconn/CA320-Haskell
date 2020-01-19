num :: Eq a => a -> [a] -> Int

num n [] = 0
num n (a:xs)
    |n == a = num n xs + 1
    |otherwise = num n xs