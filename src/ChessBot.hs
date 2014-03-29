module ChessBot (BotState(..), Command(..), runBot, parseCommand, initialBotState,
                 commandOutput, evalCommand) where

import Protocol
import Chess

data BotState = BotState {
              botGameState :: GameState
              } deriving (Eq, Show)

data Command = Move String | NewGame | Help | Board | Status | FEN
             deriving (Eq, Show)

parseCommand :: String -> Maybe Command
parseCommand ('!':commandStr) = parseCommandStr commandStr
parseCommand _ = Nothing

parseCommandStr :: String -> Maybe Command
parseCommandStr "newgame" = Just NewGame
parseCommandStr "help" = Just Help
parseCommandStr "board" = Just Board
parseCommandStr "status" = Just Status
parseCommandStr "fen" = Just FEN
parseCommandStr str | length parts == 2 && head parts == "move" = Just $ Move (parts !! 1)
        where parts = words str
parseCommandStr _ = Nothing

initialBotState :: BotState
initialBotState = undefined

commandOutput :: BotState -> Command -> (BotState, String)
commandOutput = undefined

evalCommand :: BotState -> Command -> IO BotState
evalCommand = undefined

runBot :: (Connection a) => a -> BotState -> IO BotState
runBot = undefined
