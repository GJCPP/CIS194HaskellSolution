module MoreFolds where


xor :: [Bool] -> Bool
xor xs = foldl (\x y -> (x && not y) || (not x && y)) False xs

map' :: (a -> b) -> [a] -> [b]
map' f listA = foldr (\x listB -> (f x) : listB) [] listA

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

