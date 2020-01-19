mLengths :: Int -> [Int]

leap n
    |(n `mod` 100 == 0) && (n `mod` 400 == 0) && (n `mod` 4 == 0)= True
    |(n `mod` 4 == 0) && (not (n `mod` 100 == 0)) = True
    |otherwise = False

mLengths n
    |leap n == True = [31,29,31,30,31,30,31,31,30,31,30,31]
    |otherwise = [31,28,31,30,31,30,31,31,30,31,30,31]

