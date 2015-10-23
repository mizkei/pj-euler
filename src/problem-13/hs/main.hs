import Data.List

strToInt :: String -> Int
strToInt s = read s :: Int

splitLenN :: Int -> [a] -> [[a]]
splitLenN _ [] = [[]]
splitLenN n xs = take n xs : splitLenN n (drop n xs)

addLarge :: Int -> Int -> [[Int]] -> [Int]
addLarge _ v [] = [v]
addLarge n v (x:xs) = (s `mod` p) : addLarge n (div s p) xs
    where
        s = v + sum x
        p = 10 ^ n

main = do
    contents <- readFile "data.txt"
    let nLines = lines contents
        ns = map (splitLenN 5) nLines
        nsi = reverse . transpose $ map (map strToInt . filter (/= "")) ns
    print $ (foldl1 (++) . map show . reverse . addLarge 5 0) nsi
