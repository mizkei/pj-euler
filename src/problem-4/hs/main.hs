gen3Digit :: [Int]
gen3Digit = [999, 998..100]

isPalindromic :: (Int, Int) -> Bool
isPalindromic (x, y) = xy == reverse xy
    where
        xy = show $ x * y

main = do
    print $ foldr max 0 [x * y | x <- gen3Digit, y <- gen3Digit, isPalindromic (x, y)]
