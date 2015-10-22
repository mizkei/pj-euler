factorN :: Int -> (Int, Int)
factorN n
    | even n = (div n 2, n + 1)
    | otherwise = (n, div (n + 1) 2)

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

triangleNum :: Int -> Int
triangleNum n = (length . divisors) a * (length . divisors) b
    where (a, b) = factorN n

main = print $ n * m
    where (n, m) = (factorN . head . dropWhile (\x -> triangleNum x < 500)) [1..]
