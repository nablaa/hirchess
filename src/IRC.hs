module IRC (initialize, readChannel, writeChannel) where

import Network
import System.IO
import Text.Printf
import Data.List
import Control.Concurrent

server :: String
server = "irc.freenode.org"

port :: Int
port = 6667

chan :: String
chan = "#hirchess"

nick :: String
nick = "hirchess"

user :: String
user = "hirchess"

floodDelay :: Int
floodDelay = 750

initialize :: IO Handle
initialize = do h <- connectTo server (PortNumber (fromIntegral port))
                hSetEncoding h utf8
                hSetBuffering h NoBuffering
                write h "NICK" nick
                write h "USER" (nick ++ " 0 * :" ++ user)
                write h "JOIN" chan
                return h

write :: Handle -> String -> String -> IO ()
write h s t = do
  _ <- hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t

readChannel :: Handle -> IO String
readChannel h = do t <- hGetLine h
                   let s = init t
                   putStrLn s
                   if ping s then pong s >> readChannel h else
                       if correctChannel s then
                           return (clean s)
                       else
                           readChannel h
    where clean = drop 1 . dropWhile (/= ':') . drop 1
          ping x = "PING :" `isPrefixOf` x
          pong x = write h "PONG" (':' : drop 6 x)
          correctChannel str = length (words str) >= 4 && words str !! 2 == chan

writeChannel :: Bool -> Handle -> String -> IO ()
writeChannel _ _ "" = return ()
writeChannel True h s = write h "PRIVMSG" (chan ++ " :" ++ s) >> threadDelay (floodDelay * 1000) -- prevent flooding
writeChannel False h s = write h "PRIVMSG" (chan ++ " :" ++ s)
