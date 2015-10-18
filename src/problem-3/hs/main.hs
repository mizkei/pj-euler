{- haskell.org のtopにあるプログラム -}
primes :: [Integer]
primes = filterPrime [2..]
    where filterPrime (p:xs) = 
              p : filterPrime [x | x <- xs, x `mod` p /= 0]

sqrt' :: Integer -> Integer
sqrt' = truncate . sqrt . fromIntegral

genPrimeFactors :: Integer -> [Integer]
genPrimeFactors 0 = []
genPrimeFactors n =
    (filter (\x -> n `mod` x == 0) . takeWhile (< sqrt' n)) primes

{- 一応動作するが、話しにならないほど遅い -}
main = do
    let num = 600851475143 :: Integer
    print $ foldr max 0 $ genPrimeFactors num
