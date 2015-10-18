requiredFactor :: Int -> [Int] -> Int
requiredFactor n [] = n
requiredFactor n (0:xs) = 1
requiredFactor n (x:xs)
    | n `mod` x == 0 = requiredFactor (div n x) xs
    | otherwise = requiredFactor n xs

divPossible :: Int -> Int -> Int
divPossible _ 0 = 1
divPossible n d
    | n `mod` d == 0 = div n d
    | otherwise = n

{- 2からnまでの数字で割ることのできる数値のリスト(?) -}
{- ex) 10 -> [2, 3, 2, 5, 7, 2, 3] -}
ndList :: Int -> [Int]
ndList n = removeOverlap [2..n]
    where
        removeOverlap [] = []
        removeOverlap (x:xs) =
            x:removeOverlap[v | a <- xs, let v = divPossible a x, v /= 1]

main = do
    print $ foldr (*) 1 $ ndList 20
