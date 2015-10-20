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

{- 配列をずらして、上三角や下三角行列を生成し、斜めを表現する -}
{- なんだか異様に長い -}
{- もっと綺麗な実装を -}
triangularMatrix :: (Int -> [a] -> [a]) -> (Int -> Int) -> Int -> [[a]] -> [[a]]
triangularMatrix _ _ _ [] = []
triangularMatrix fm f n (xs:xss) = fm n xs : triangularMatrix fm f (f n) xss

main = do
    contents <- readFile "grid.txt"
    let nLines = map words $ lines contents
        nLists = map (map strToInt) nLines
        len = length $ head nLists
    let a1 = maxAdjacent4 nLists
        a2 = maxAdjacent4 $ transpose nLists
        a3 = maxAdjacent4 $ transpose $ map reverse $ triangularMatrix take (+ 1) 0 nLists
        a4 = maxAdjacent4 $ transpose $ map reverse $ triangularMatrix take (subtract 1) (len - 1) nLists
        a5 = maxAdjacent4 $ transpose $ triangularMatrix drop (+ 1) 0 nLists
        a6 = maxAdjacent4 $ transpose $ triangularMatrix drop (subtract 1) (len - 1) nLists
    print $ maximum [a1, a2, a3, a4, a5, a6]
