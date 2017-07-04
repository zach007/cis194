module Week2 where

import Log

parseMessage :: String -> LogMessage
parseMessage xs= let list = words xs in
                 case list of
                 ("I":line:msg) -> LogMessage Info (read line) (unwords msg)
                 ("W":line:msg) -> LogMessage Warning (read line) (unwords msg)
                 ("E":line:level:msg) -> LogMessage (Error (read line)) (read level) (unwords msg)
                 _ -> Unknown (unwords list)


parse :: String -> [LogMessage]
--parse xs = map parseMessage $ lines xs
parse = map parseMessage . lines



insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ t1 _) (Node left msg2@(LogMessage _ t2 _) right)
  | t1 < t2 = Node (insert msg1 left) msg2 right
  | otherwise = Node left msg2 (insert msg1 right)
insert _ tree = tree
