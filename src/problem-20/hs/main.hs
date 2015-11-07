import Data.Char

factor :: Integer -> Integer -> Integer
factor a n
    | n == 1 = a
    | otherwise = factor (a * n) (n - 1)

main = print $ (sum . map digitToInt . show . factor 1) 100
