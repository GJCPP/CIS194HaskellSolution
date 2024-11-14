{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Text.Read

isInt :: String -> Bool
isInt s = (readMaybe s :: Maybe Int) /= Nothing

areInt :: [String] -> Bool
areInt [] = True
areInt s = (readMaybe (head s) :: Maybe Int) /= Nothing && areInt (drop 1 s)

takeAreInt :: Int -> [String] -> Bool
takeAreInt i s = areInt (take i s)

firstInt :: [String] -> Int
firstInt s = read (head s) :: Int

secondInt :: [String] -> Int
secondInt s = read (s !! 1) :: Int

parseWords :: [String] -> LogMessage
parseWords ("E" : s)
    | length s < 2   = Unknown $ unwords ("I" : s)
    | takeAreInt 2 s = LogMessage (Error (firstInt s)) (secondInt s) (unwords (drop 2 s))
    | otherwise      = Unknown $ unwords ("I" : s)
parseWords ("I" : s)
    | length s < 1   = Unknown $ unwords ("I" : s)
    | takeAreInt 1 s = LogMessage Info (firstInt s) (unwords (drop 1 s))
    | otherwise      = Unknown $ unwords ("I" : s)
parseWords ("W" : s)
    | length s < 1   = Unknown $ unwords ("W" : s)
    | takeAreInt 1 s = LogMessage Info (firstInt s) (unwords (drop 1 s))
    | otherwise      = Unknown $ unwords ("W" : s)
parseWords x = Unknown $ unwords x

parseMessage :: String -> LogMessage
parseMessage s = parseWords $ words s

parse :: String -> [LogMessage]
parse s = parseLines $ lines s
    where parseLines x
            | x == []   = []
            | otherwise = (parseMessage (head x)) : parseLines (drop 1 x)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ stamp _) (Node left nowMessage@(LogMessage _ oldstamp _) right)
    | stamp < oldstamp = Node (insert message left) nowMessage right
    | stamp > oldstamp = Node left nowMessage (insert message right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (firstMessage : remainder) = insert firstMessage (build remainder)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = concat [inOrder left, [msg], inOrder right]

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error weight) _ msg) : remainder)
            | weight < 50   = whatWentWrong remainder
            | otherwise     = msg : whatWentWrong remainder
whatWentWrong (_ : remainder) = whatWentWrong remainder
