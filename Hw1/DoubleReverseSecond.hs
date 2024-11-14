module Test where

reverseList :: [a] -> [a]
reverseList [] = []
reverseList x = (last x) : (reverseList $ init x)

doubleSecond :: Num a => [a] -> [a]
doubleSecond [] = []
doubleSecond (x : []) = [x]
doubleSecond (x : (y : z)) = x : (2*y : doubleSecond z)

doubleReverseSecond :: Num a => [a] -> [a]
doubleReverseSecond x = reverseList $ doubleSecond $ reverseList x

sumDigit :: Integer -> Integer
sumDigit x
    | x < 10 = x
    | otherwise = (x `mod` 10) + (sumDigit $ x `div` 10)

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x : xs) = (sumDigit x) + (sumList xs)

sumCard :: [Integer] -> Integer
sumCard xs = sumList $ doubleReverseSecond xs
