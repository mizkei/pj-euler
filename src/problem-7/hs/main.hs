primes :: [Integer]
primes = filterPrime[2..]
    where
        filterPrime (x:xs) = x:filterPrime[a | a <- xs, a `mod` x /= 0]

main = do
    print $ primes !! 10000
