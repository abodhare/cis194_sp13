{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage = resolve . words
    where resolve ("I":ts:msg) = LogMessage Info (read ts) (unwords msg)
          resolve ("W":ts:msg) = LogMessage Warning (read ts) (unwords msg)
          resolve ("E":lvl:ts:msg) = LogMessage (Error (read lvl)) (read ts) (unwords msg)
          resolve _ = Unknown "Not a log message"

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) x = x
insert x Leaf = Node Leaf x Leaf
insert msg@(LogMessage _ ts _) (Node x nodeMsg@(LogMessage _ nodeTs _) y)
    | ts <= nodeTs = Node (insert msg x) nodeMsg y
    | ts > nodeTs = Node x nodeMsg (insert msg y)
insert (LogMessage _ _ _) x = x

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node x y z) = inOrder x ++ [y] ++ inOrder z

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = takeImportant . inOrder . build
    where takeImportant = extract . filter severeError
          severeError (LogMessage (Error x) _ _)
            | x >= 50 = True
            | x < 50 = False
            | otherwise = False
          severeError (LogMessage Info _ _) = False
          severeError (LogMessage Warning _ _) = False
          severeError (Unknown _) = False
          extract [] = []
          extract ((LogMessage _ _ x):xs) = x : (extract xs)
          extract ((Unknown _):x) = extract x

firstFewMessages :: TimeStamp -> [LogMessage] -> [String]
firstFewMessages ts msgs = takeImportant ts (inOrder (build msgs))
    where takeImportant x logMsgs = extract (takeWhile (important x) logMsgs)
          important x (LogMessage _ msgTs _)
              | msgTs <= x = True
              | otherwise = False
          important _ _ = False
          extract [] = []
          extract ((LogMessage _ _ x):xs) = x : (extract xs)
          extract ((Unknown _):xs) = extract xs