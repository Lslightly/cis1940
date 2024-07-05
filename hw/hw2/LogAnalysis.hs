{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List (isInfixOf)

parseMessage :: String -> LogMessage
index :: String -> Integer -> String
index s n = words s !! fromIntegral n
parseMessage s
  | index s 0 == "E" = LogMessage (Error (read (index s 1))) (read (index s 2)) (unwords (drop 3 (words s)))
  | index s 0 == "I" = LogMessage Info (read (index s 1)) (unwords (drop 2 (words s)))
  | index s 0 == "W" = LogMessage Warning (read (index s 1)) (unwords (drop 2 (words s)))
  | otherwise = Unknown s

parse :: String -> [LogMessage]
parse s = 
    map parseMessage (lines s)

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ ts _) = ts
getTimeStamp (Unknown _) = error "Unknown LogMessage"
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage (Node left nodeLogMessage right)
  | getTimeStamp logMessage < getTimeStamp nodeLogMessage = Node (insert logMessage left) nodeLogMessage right
  | otherwise = Node left nodeLogMessage (insert logMessage right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build xs = foldr insert Leaf xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) = inOrder left ++ [logMessage] ++ inOrder right

severeError :: LogMessage -> Bool
severeError (LogMessage (Error severity) _ _) = severity >= 50
severeError _ = False
getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message) = message
getMessage (Unknown message) = message

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong l = map getMessage (filter severeError (inOrder (build l)))

isErrorMsg :: LogMessage -> Bool
isErrorMsg (LogMessage (Error severity) _ _) = severity < 50 && severity > 40
isErrorMsg _ = False

whatWentWrongFilter :: (LogMessage -> Bool) -> [LogMessage] ->  [String]
whatWentWrongFilter fn l = map getMessage (filter fn (inOrder (build l)))