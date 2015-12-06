import Data.List

makeComb :: Int -> String -> String -> [String]
makeComb _ x [] = [x]
makeComb n x str
    | n < length str = makeComb 0 nstr dstr ++ makeComb (n + 1) x str
    | otherwise = []
    where
        num = str !! n
        dstr = delete num str
        nstr = x ++ [num]

main = print $ makeComb 0 "" "0123456789" !! 999999
