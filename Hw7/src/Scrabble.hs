{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Sized
import Data.Char
import JoinList

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Sized Score where
    size (Score s) = Size s

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty = Score 0

intScore :: Char -> Int
intScore a
    | a == 'd' = 2
    | a == 'b' || a == 'c' || a == 'm' ||
      a == 'p' = 3
    | a == 'f' || a == 'h' || a == 'v' ||
      a == 'w' || a == 'y' = 4
    | a == 'k' = 5
    | a == 'j' = 8
    | a == 'x' = 8
    | a == 'z' = 10
    | isAsciiLower a = 1
    | otherwise = 0

score :: Char -> Score
score = Score . intScore

scoreLine :: String -> JoinList Score String
scoreLine s = Single sc s
    where sc = sum . map score $ s
