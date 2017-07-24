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


build :: [LogMessage] -> MessageTree
{--point full style
build [] = Leaf
build (msg : xsmsg) = insert msg (build xsmsg)
--}
{--haskell is very easy to abstract a function, foint free style : doesn't care about the data--}
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree msg rightTree) = inOrder leftTree ++ [msg] ++ inOrder rightTree

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extractMessage . inOrder . build . filter (filterMessage 50)
-- filter the error logmessage and then filter > 50 error

filterMessage :: Int -> LogMessage -> Bool
filterMessage minError (LogMessage (Error lvl) _ _)
  | minError <= lvl = True
  | otherwise = False
filterMessage _ _ = False

extractMessage :: [LogMessage]-> [String]
extractMessage (LogMessage _ _ info : msgs) = info : extractMessage msgs
extractMessage _ = []
