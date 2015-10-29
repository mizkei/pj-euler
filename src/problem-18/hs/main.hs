strToInt :: String -> Int
strToInt s = read s :: Int

spAry :: [Int] -> [Int]
spAry ls = zipWith max hls lsl
    where
        hls = head ls : ls
        lsl = ls ++ [last ls]

maxPath :: [[Int]] -> Int
maxPath [] = 0
maxPath [x] = maximum x
maxPath ls@(x:y:xs)
    | length ls == 1 = maximum $ head ls
    | otherwise = maxPath $ zipWith (+) (spAry x) y : xs

main = do
    contents <- readFile "data.txt"
    let nLines = map (map strToInt . words) $ lines contents
    print $ maxPath nLines
