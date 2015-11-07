leapYear :: Int -> Bool
leapYear year
    | divisible 4 && not (divisible 100) || divisible 400 = True
    | otherwise = False
    where
        divisible x = mod year x == 0

monthPlus :: [Int]
monthPlus = [3, 0, 3, 2, 3, 2, 3, 3, 2, 3, 2, 3]

leapWeek :: (Int, Int) -> Int
leapWeek (y, m)
    | leapYear y && m == 2 = 1
    | otherwise = 0

firstDay :: (Int, Int, Int) -> [(Int, Int, Int)]
firstDay x@(y, m, w) = x : firstDay (nextY, nextM, nextW)
    where
        nextY = if m == 12 then y + 1 else y
        nextM = mod m 12 + 1
        nextW = mod (w + monthPlus !! (m - 1) + leapWeek (y, m)) 7

main = print $ (length . filter (\(y, _, w) -> w == 0 && 1900 < y) . takeWhile (\(y, _, _) -> y < 2001)) $ firstDay (1900, 1, 1)
