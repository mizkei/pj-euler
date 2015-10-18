sumDivisibleNum :: Int -> Int -> Int
sumDivisibleNum 0 _ = 0
sumDivisibleNum _ 0 = 0
sumDivisibleNum n d = sum [x | x <- [1..n], x `mod` d == 0]

main = do
    print $ sumDivisibleNum 999 3 + sumDivisibleNum 999 5 - sumDivisibleNum 999 15
