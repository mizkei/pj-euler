main = print $ head [a * b * c | a <- [1..998], b <- [1..a], c <- [1..b], a + b + c == 1000, a ^ 2 == b ^ 2 + c ^ 2]
