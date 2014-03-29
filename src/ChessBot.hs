module ChessBot (BotState(..), Command(..), runBot, parseCommand, initialBotState,
                 commandOutput, evalCommand) where

import Protocol
import Chess
import Chess.FEN
import UI

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
initialBotState = BotState newGame

commandOutput :: BotState -> Command -> (BotState, String)
commandOutput _ NewGame = (initialBotState, "New game started")
commandOutput bot Help = (bot, unlines helpText)
commandOutput bot@(BotState game) Board = (bot, printBoard (board game))
commandOutput bot@(BotState game) Status = (bot, printColoredState game)
commandOutput bot@(BotState game) FEN = (bot, "FEN: " ++ writeFEN game)
commandOutput bot@(BotState game) (Move moveStr) = case move game moveStr of
                                                           Just game' -> (bot { botGameState = game' }, "Moved: " ++ moveStr)
                                                           Nothing -> (bot, "Invalid move: " ++ moveStr)

evalCommand :: BotState -> Command -> IO BotState
evalCommand = undefined

runBot :: (Connection a) => a -> BotState -> IO BotState
runBot = undefined

helpText :: [String]
helpText = ["Available commands:"
           , "!move MOVE    Makes a move. Move is given in coordinate notation. Example: \"!move b1-c3\"."
           , "!newgame      Starts a new game."
           , "!board        Prints the current game board."
           , "!status       Prints the current game status."
           , "!fen          Prints out the FEN notation of the current game status."]
