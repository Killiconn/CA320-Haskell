leap :: Int -> Bool


leap n
    |(n `mod` 100 == 0) && (n `mod` 400 == 0) && (n `mod` 4 == 0)= True
    |(n `mod` 4 == 0) && (not (n `mod` 100 == 0)) = True
    |otherwise = False