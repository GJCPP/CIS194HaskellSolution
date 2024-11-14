module LocalMaxima where

-- This function makes adjacent three elements into one tuple
-- E.g. [1, 2, 3, 4] -> [(1, 2, 3), (2, 3, 4)]
triplize :: [a] -> [(a, a, a)]
triplize (x : (y : (z : remainder))) = (x, y, z) : triplize (y : (z : remainder))
triplize _ = []

-- Given tryple (x, y, z), this function checks if y is local maxima.
isLocalMaxima :: Ord a => (a, a, a) -> Bool
isLocalMaxima (x, y, z) = y > x && y > z

-- First filter out all (x, y, z) with y > x and y > z, then map such triple to y.
localMaxima :: Ord a => [a] -> [a]
localMaxima xs = map (\(x, y, z) -> y) (filter isLocalMaxima (triplize xs))
