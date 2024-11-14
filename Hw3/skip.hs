module Skip where
import Data.Maybe

-- This function gives index to each element in list.
number :: [a] -> [(Int, a)]
number [] = []
number x  = zip [1..] x

-- This function filter out elements with index non-divisible to k
keepEachK :: Int -> [a] -> [a]
keepEachK 0 _  = []
keepEachK k x  = mapMaybe (\(i :: Int, j :: a) -> if i `mod` k == 0 then Just j else Nothing) (number x)
-- The in-line function checks if index is divisible by k

-- Implement skips!
skips :: [a] -> [[a]]
skips x = map (\i -> keepEachK i x) [1..length x]
