primes :: [Integer]
primes = 2:filterPrime [3] [3,5..]
    where
        filterPrime (p:xs) ys = 
            let (px, ps) = span (< p ^ 2) ys
                in px ++ filterPrime (xs ++ px) [a | a <- ps, a `mod` p /= 0]

main = print $ primes !! 10000
