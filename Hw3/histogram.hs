module Histogram where

-- Input (k, []), add the k-th entry (start from zero) by 1.
add :: Integer -> [Integer] -> [Integer]
add _ [] = []
add 0 (x : rem) = (x + 1) : rem
add k (x : rem) = x : add (k - 1) rem

-- Count the occurances of all integers.
count :: [Integer] -> [Integer] -> [Integer]
count [] cnt = cnt
count (x : rem) cnt = count rem (add x cnt)

-- Check if the counter is all zero (so the iteration may end).
isAllZero :: [Integer] -> Bool
isAllZero xs = all (\x -> x == 0) xs

-- Put "*" to non-zero positions, whitespaces to zero positions.
starNonZero :: [Integer] -> [Char]
starNonZero [] = []
starNonZero (x : rem) = (if x > 0 then '*' else ' ') : starNonZero rem

sub :: [Integer] -> [Integer]
sub xs = map (\x -> if x > 0 then x - 1 else x) xs

newline :: String -> String
newline s = s ++ "\n"

maxNumber = 10
zeroCounter = replicate maxNumber 0
baseOutput = concat [newline $ concat (replicate maxNumber "="), newline $ concat (map show [0..maxNumber-1])]

-- This function builds the star-bar in figure, down to top.
printLines :: [Integer] -> [String]
printLines cnt
    | isAllZero cnt = []
    | otherwise = newline (starNonZero cnt) : printLines (sub cnt)
    -- First generate a newline with star on non-zero position
    -- Then sub
histogram :: [Integer] -> String
histogram xs = (concat $ reverse $ printLines (count xs zeroCounter)) ++ baseOutput

