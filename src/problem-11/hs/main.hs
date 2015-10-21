import Data.List

strToInt :: String -> Int
strToInt = read

maxAdjacentNum :: Int -> [Int] -> Int
maxAdjacentNum n ls@(_:xs)
    | length ls < n = 0
    | length ls == n = product ls
    | otherwise = max ((product . take n) ls) (maxAdjacentNum n xs)

maxAdjacent4 :: [[Int]] -> Int
maxAdjacent4 = maximum . map (maxAdjacentNum 4)

rhombus :: Int -> Int -> [[Int]] -> [[Int]]
rhombus _ _ [] = [[]]
rhombus n ln (x:xs)
    | n < 0 || ln < n = [[]]
    | otherwise = (replicate n 0 ++ x ++ replicate (ln - n) 0) : rhombus (n + 1) ln xs

main = do
    contents <- readFile "grid.txt"
    let nLines = map words $ lines contents
        nLists = map (map strToInt) nLines
        len = length $ head nLists
    let a1 = maxAdjacent4 nLists
        a2 = maxAdjacent4 $ transpose nLists
        a3 = maxAdjacent4 $ transpose $ rhombus 0 (len - 1) nLists
        a4 = maxAdjacent4 $ transpose $ rhombus 0 (len - 1) $ reverse nLists
    print $ maximum [a1, a2, a3, a4]
