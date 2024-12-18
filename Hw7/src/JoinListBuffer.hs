{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
module JoinListBuffer where

import Buffer
import JoinList
import Scrabble
import Sized
import Data.List.Split

newtype JLBuffer = JLBuffer (JoinList (Score, Size) String)

instance Buffer JLBuffer where
    toString :: JLBuffer -> String
    toString (JLBuffer Empty) = ""
    toString (JLBuffer (Single _ s)) = s
    toString (JLBuffer (Append _ x y)) = toString' x ++ "\n" ++ toString' y
        where toString' = toString . JLBuffer

    fromString :: String -> JLBuffer
    fromString "" = JLBuffer Empty
    fromString s  = JLBuffer $ loader $ map (\line -> Single (scoreStr line, 1) line) x
        where x = splitOn "\n" s
              scoreStr = sum . map score
              loader :: [JoinList (Score, Size) String] -> JoinList (Score, Size) String
              loader [] = Empty
              loader lst = if length lst > 1 then first +++ last else head lst -- Balance loader
                where first  = loader . take len $ lst
                      last   = loader . drop len $ lst
                      len    = div (length lst) 2

    value (JLBuffer list)= intScore . fst $ tag list
        where intScore (Score sc) = sc

    line i (JLBuffer list) = indexJ i list

    numLines (JLBuffer list) = intSize . snd . tag $ list

    replaceLine n l (JLBuffer jl) = JLBuffer $ takeJ n jl +++ newline +++ dropJ (n+1) jl
        where newline = Single (scoreStr l, Size 1) l
              scoreStr = sum . map score
        