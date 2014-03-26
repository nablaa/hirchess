module Main (main) where

import Control.Monad
import Protocol
import IRC
import ChessBot

ircConfig :: IRC
ircConfig = IRC {
        ircServer = "irc.freenode.org",
        ircPort = 6667,
        ircChannel = "#hirchess",
        ircNick = "hirchess",
        ircUser = "hirchess",
        ircHandle = Nothing
}

main :: IO ()
main = do connection <- connect ircConfig
          void $ runBot connection initialBotState
