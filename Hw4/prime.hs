module PrimeSieve where

import Data.Maybe
import Data.Array

cross :: Integer -> Array Integer Bool
cross n = array
    (1, n) $ [(i, True) | i <- [1..n]] ++ [(ind i j, False) | i <- [1..n], j <- takeWhile (\x -> ind i x <= n) [i..]]
    where
        ind i j = i + j + 2 * i * j

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = mapMaybe func $ assocs (cross n)
    where func (x, y) = if y then Just (2 * x + 1) else Nothing
