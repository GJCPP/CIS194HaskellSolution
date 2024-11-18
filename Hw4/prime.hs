module PrimeSieve where

import Data.List
import qualified Data.HashSet as HashSet
import qualified Data.IntSet as IntSet

comp :: (Num a, Ord a) => [a] -> [a] -> [a]
comp xs ys = [ x + y + 2 * x * y | x <- xs, y <- ys, x <= y ]


sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x * 2 + 1) ([1..n] \\ cross n)
    where cross n = comp [1..n] [1..n]
    -- The operator "\\" has complexity $O(nm)$, which makes it $O(n^3) and is terrible for large input, but anyway...

sieveSundaram' n = map (\x -> x * 2 + 1) . filter (\x -> not $ HashSet.member x (cross n)) $ [1..n]
    where cross n = HashSet.fromList $ comp [1..n] [1..n]
    -- Note: HashSet required.

sieveSundaram'' n = map (\x -> x * 2 + 1) . filter (\x -> not $ IntSet.member x (cross n)) $ [1..n]
    where cross n = IntSet.fromList $ comp [1..n] [1..n]

