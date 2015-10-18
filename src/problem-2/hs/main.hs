{- wikipediaを参考にした -}
fib :: [Int]
fib = 1:1:zipWith (+) fib (tail fib)

main = do
    print $ (sum . filter even . takeWhile (< 4000000)) fib
