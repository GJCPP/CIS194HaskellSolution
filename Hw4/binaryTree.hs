module BinaryTree where



data Tree a = Leaf
        | Node Integer (Tree a) a (Tree a)
        deriving (Show, Eq)


addToTree :: Tree a -> a -> Tree a
addToTree Leaf a = Node 0 Leaf a Leaf
addToTree (Node dep left val right) newval
    | (goLeft left right) == True    = Node (newDepth left right) (addToTree left newval) val right
    | otherwise                      = Node (newDepth left right) left val (addToTree right newval)
    where
        goLeft :: Tree a -> Tree a -> Bool
        goLeft x y = (getHeight x) < (getHeight y)

        newDepth :: Tree a -> Tree a -> Integer
        newDepth x y = 1 + max (getHeight x) (getHeight y)

        getHeight :: Tree a -> Integer
        getHeight Leaf = 0
        getHeight (Node x _ _ _) = x

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs = foldl addToTree Leaf xs
