module Wholemeal where


-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
-- | even x = (x - 2) * fun1 xs
-- | otherwise = fun1 xs

fun1 :: [Integer] -> Integer
fun1 xs = foldl (\x y -> if even y then x * (y - 2) else x) 1 xs

-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n
--     | even n = n + fun2 (n `div` 2)
--     | otherwise = fun2 (3 * n + 1)

fun2 :: Integer -> Integer
fun2 n = sum . takeWhile (> 1) . iterate hailStone $ n
    where hailStone n = if even n then n `div` 2 else 3 * n + 1
