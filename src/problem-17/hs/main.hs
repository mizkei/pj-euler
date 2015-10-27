numToStr :: Int -> String
numToStr 1 = "one"
numToStr 2 = "two"
numToStr 3 = "three"
numToStr 4 = "four"
numToStr 5 = "five"
numToStr 6 = "six"
numToStr 7 = "seven"
numToStr 8 = "eight"
numToStr 9 = "nine"
numToStr 10 = "ten"
numToStr 11 = "eleven"
numToStr 12 = "twelve"
numToStr 13 = "thirteen"
numToStr 14 = "fourteen"
numToStr 15 = "fifteen"
numToStr 16 = "sixteen"
numToStr 17 = "seventeen"
numToStr 18 = "eighteen"
numToStr 19 = "nineteen"
numToStr 20 = "twenty"
numToStr 30 = "thirty"
numToStr 40 = "forty"
numToStr 50 = "fifty"
numToStr 60 = "sixty"
numToStr 70 = "seventy"
numToStr 80 = "eighty"
numToStr 90 = "ninety"
numToStr n = ""

numTextAB :: Int -> String
numTextAB 0 = ""
numTextAB 10 = "onethousand"
numTextAB n = numToStr n ++ "hundred"

numTextCD :: Int -> String
numTextCD 0 = ""
numTextCD n
    | n < 21 || mod n 10 == 0 = numToStr n
    | otherwise = numToStr c ++ numToStr d
    where
        c = div n 10 * 10
        d = mod n 10

countNumWord :: Int -> String
countNumWord n =
    let (ab, cd) = (div n 100, mod n 100)
        link = if ab /= 0 && cd /= 0 then "and" else ""
    in numTextAB ab ++ link ++ numTextCD cd

main = print $ (sum . map (length . countNumWord)) [1..1000]
