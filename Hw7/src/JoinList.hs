{-# LANGUAGE InstanceSigs #-}
module JoinList where

import Sized

-- Exercise 1
data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)
    
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y



-- Exercise 2
instance (Sized m, Monoid m) => Sized (JoinList m a) where
    size = size . tag

intSize :: Sized m => m -> Int
intSize = extractInt . size
    where extractInt (Size i) = i

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Append s x y)
    | size s <= ind || i < 0 = Nothing
    | size x > ind = indexJ i x
    | otherwise = indexJ (intSize $ ind - size x) y
    where ind = Size i
indexJ _ Empty = Nothing
indexJ i (Single s v) = if i == 0 then Just v else Nothing


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i lst@(Single s _) = if Size i >= size s then Empty else lst
dropJ i lst@(Append s x y)
    | Size i >= sz = Empty
    | i <= 0       = lst
    | otherwise    = nx +++ ny
    where sz = size s
          nx = dropJ i x
          ny = dropJ (i - intSize x) y

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i lst@(Single s _) = if Size i >= size s then lst else Empty
takeJ i lst@(Append s x y)
    | i <= 0 = Empty
    | i >= intSize s = lst
    | otherwise    = nx +++ ny
    where nx = takeJ i x
          ny = takeJ (i - intSize x) y

-- Comment: Can we compress these three functions?



