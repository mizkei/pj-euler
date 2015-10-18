import System.IO
import Data.Char

productChars :: [Char] -> Int
productChars = foldl (*) 1 . map digitToInt

{- Maybe Intを返り値にしたほうがよいかもしれません -}
{- otherwiseの括弧を減らしたい -}
maxAdjacentProduct :: Int -> [Char] -> Int
maxAdjacentProduct n s@(_:ss)
    | length s < n = 1
    | length s == n = productChars s
    | otherwise = max (productChars (take 13 s)) (maxAdjacentProduct n ss)

main = do
    contents <- readFile "nums.txt"
    let nLine = lines contents
        nums = foldl (++) "" nLine
    print $ maxAdjacentProduct 13 nums
