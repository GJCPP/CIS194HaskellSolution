module PrimeSieve where

import Data.List

comp :: [Integer] -> [Integer] -> [Integer]
comp xs ys = [ x + y + 2 * x * y | x <- xs, y <- ys, x <= y ]


sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x * 2 + 1) ([1..(2*n+2)] \\ cross n)
    where cross n = comp [1..n] [1..n]

-- The operator "\\" has complexity $O(nm)$, which is terrible for large input, but anyway...

