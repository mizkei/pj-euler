import Data.List.Split

sortName :: [String] -> [String]
sortName [] = []
sortName (x:xs) = sortName small ++ [x] ++ sortName large
    where
        small = filter (< x) xs
        large = filter (>= x) xs

alpha :: [(Int, Char)]
alpha = zip [1..] ['A'..'Z']

scoreAlpha :: Char -> Int
scoreAlpha c = f c alpha
    where
        f _ [] = 0
        f s ((n, k):xs)
            | s == k = n
            | otherwise = f s xs

scoreName :: String -> Int
scoreName = sum . map scoreAlpha

replaceQt :: String -> String
replaceQt [] = []
replaceQt (x:xs)
    | x == '"' = replaceQt xs
    | otherwise = x:replaceQt xs


main = do
    contents <- readFile "p022_names.txt"
    let (names:_) = lines contents
    print $ (sum . map (\(n, s) -> n * scoreName s) . zip [1..] . sortName . splitOn "," . replaceQt) names
