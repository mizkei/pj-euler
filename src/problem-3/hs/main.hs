primes :: [Integer]
primes = 2:filterPrime [3] [3,5..]
    where 
        filterPrime (p:xs) ys = 
            let (px, ps) = span (< p ^ 2) ys
                in px ++ filterPrime (xs ++ ps) [x | x <- ps, x `mod` p /= 0]

sqrt' :: Integer -> Integer
sqrt' = truncate . sqrt . fromIntegral

genPrimeFactors :: Integer -> [Integer]
genPrimeFactors 0 = []
genPrimeFactors n =
    (filter (\x -> n `mod` x == 0) . takeWhile (< sqrt' n)) primes

main = do
    let num = 600851475143 :: Integer
    print $ foldr max 0 $ genPrimeFactors num
