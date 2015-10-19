primes :: [Integer]
primes = 2 : f [3] [3,5..]
    where
        f (p:xs) ys = 
            let (px, ps) = span (< p ^ 2) ys
                in px ++ f (xs ++ px) [x | x <- ps, x `mod` p /= 0]

main = print $ (sum . takeWhile (< 2000000)) primes
