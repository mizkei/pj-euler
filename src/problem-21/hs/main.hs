factor :: Int -> [Int]
factor n = [x | x <- [1..n], mod n x == 0]

sumFactor :: Int -> Int
sumFactor = sum . factor

amicable :: Int -> Bool
amicable n
    | 9999 < m || m == n = False
    | n == sumFactor m - m = True
    | otherwise = False
    where
        m = sumFactor n - n

main = print $ (sum . filter amicable) [1..9999]
