dup :: Eq a => [a] -> Bool

dup [] = False
dup [x] = False
dup (x:y:xs)
    |x == y = True
    |check x (y:xs) == False = dup (y:xs)
    |check x (y:xs) == True = True

check x [] = False
check x (z:xs)
    |x == z = True
    |otherwise = check x (xs)