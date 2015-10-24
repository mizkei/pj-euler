factorical :: Integer -> Integer -> Integer -> Integer
factorical n a m = f n a
    where
        f x xs
            | x == m = xs
            | otherwise = f (x - 1) (xs * x)

factorical' :: Integer -> Integer -> Integer
factorical' n a = factorical n a 1

factorical'' :: Integer -> Integer
factorical'' n = factorical n 1 1

combination :: Integer -> Integer -> Integer
combination n m = div (factorical n 1 (n - m)) (factorical'' m)

main = print $ combination 40 20
