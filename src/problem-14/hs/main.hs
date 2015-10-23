{- collatzのtermを求めているが、長さだけ求めるようにすれば、早くなるかも -}
collatz :: [Int] -> Int -> [Int]
collatz ls n
    | n == 1 = n : ls
    | even n = collatz (n:ls) (div n 2)
    | otherwise = collatz (n:ls) (n * 3 + 1)

maxCollatzLen :: (Int, Int) -> [Int] -> (Int, Int)
maxCollatzLen m [] = m
maxCollatzLen ap@(m, ml) (x:xs)
    | ml < nl = maxCollatzLen bp xs
    | otherwise = maxCollatzLen ap xs
    where bp@(n, nl) = (x, length (collatz [] x))

main = do
    let (mx, mxl) = maxCollatzLen (0, 0) [1..1000000]
    print mx
