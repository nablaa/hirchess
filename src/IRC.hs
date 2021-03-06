module IRC (IRC(..)) where

import Protocol
import Network
import System.IO
import Text.Printf
import Data.List
import Data.Maybe
import Control.Concurrent
import Control.Exception (try)

data IRC = IRC {
        ircServer :: String
      , ircPort :: Int
      , ircChannel :: String
      , ircNick :: String
      , ircUser :: String
      , ircHandle :: Maybe Handle
      }

instance Connection IRC where
        connect = ircConnect
        disconnect = ircDisconnect
        readMessage = ircReadMessage
        writeMessage = ircWriteMessage

floodDelay :: Int
floodDelay = 750

ircConnect :: IRC -> IO IRC
ircConnect irc@(IRC server port channel nick user _) =
        do h <- connectTo server (PortNumber (fromIntegral port))
           hSetEncoding h char8
           hSetBuffering h NoBuffering
           write h "NICK" nick
           write h "USER" (nick ++ " 0 * :" ++ user)
           write h "JOIN" channel
           return irc { ircHandle = Just h }

ircDisconnect :: IRC -> IO IRC
ircDisconnect irc = do hClose $ fromJust $ ircHandle irc
                       return irc { ircHandle = Nothing }

ircReadMessage :: IRC -> IO String
ircReadMessage irc = do t <- try (hGetLine h) :: IO (Either IOError String)
                        case t of
                                Left e -> do putStrLn $ "Error reading line: " ++ show e
                                             ircReadMessage irc
                                Right line -> ircReadLine irc line
        where h = fromJust $ ircHandle irc

ircReadLine :: IRC -> String -> IO String
ircReadLine irc line = do let s = init line
                          putStrLn s
                          if ping s
                                  then pong s >> ircReadMessage irc
                                  else if correctChannel s
                                               then return (clean s)
                                               else ircReadMessage irc
        where h = fromJust $ ircHandle irc
              clean = drop 1 . dropWhile (/= ':') . drop 1
              ping x = "PING :" `isPrefixOf` x
              pong x = write h "PONG" (':' : drop 6 x)
              correctChannel str = length (words str) >= 4 && words str !! 2 == ircChannel irc

ircWriteMessage :: IRC -> String -> IO ()
ircWriteMessage _ "" = return ()
ircWriteMessage irc str = mapM_ (writeOneLine h channel) (lines str)
        where h = fromJust $ ircHandle irc
              channel = ircChannel irc

writeOneLine :: Handle -> String ->  String -> IO ()
writeOneLine handle channel str = write handle "PRIVMSG" (channel ++ " :" ++ str) >> threadDelay (floodDelay * 1000) -- prevent flooding

write :: Handle -> String -> String -> IO ()
write h s t = do
  _ <- hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t
