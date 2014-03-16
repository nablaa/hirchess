module Main (main) where

import Chess
import Chess.FEN
import System.IO
import Data.List
import Data.Char
import IRC
import UI
import Data.Maybe

data Command = Players | AddPlayer Color String | RemovePlayer Color String
             | PrintBoard | PrintUnicodeBoard | PrintCompactBoard | PrintFEN
             | PrintStatus | Help | MakeMove String | Undo | NewGame
             | ClaimDraw | Invalid
               deriving (Eq, Show)

data BotState = BotState {
      botStateGame :: GameState
    , whitePlayers :: [String]
    , blackPlayers :: [String]
    , commandHistory :: [Command]
    } deriving (Eq, Show)

initialBotState :: BotState
initialBotState = BotState newGame [] [] []

parseCommand :: String -> Maybe Command
parseCommand str | cmd "!players" = Just Players
                 | cmd "!player" && count >= 3 && arg 1 == "add" = case player of
                                                                 Just p -> Just $ AddPlayer p (arg 3)
                                                                 Nothing -> Nothing
                 | cmd "!player" && count >= 3 && arg 1 == "remove" = case player of
                                                                 Just p -> Just $ AddPlayer p (arg 3)
                                                                 Nothing -> Nothing
                 | cmd "!uboard" = Just PrintUnicodeBoard
                 | cmd "!cboard" = Just PrintCompactBoard
                 | cmd "!board" = Just PrintBoard
                 | cmd "!fen" = Just PrintFEN
                 | cmd "!status" = Just PrintStatus
                 | cmd "!move" && count >= 2 && arg 1 == "draw" = Just ClaimDraw
                 | cmd "!move" && count >= 2 = Just $ MakeMove $ arg 1
                 | cmd "!undo" = Just Undo
                 | cmd "!newgame" = Just NewGame
                 | cmd "!help" = Just Help
                 | cmd "!" = Just Invalid
    where cmd c = c `isPrefixOf` str
          w = words str
          count = length w
          arg n = map toLower $ w !! n
          player = case arg 2 of
                     "black" -> Just Black
                     "white" -> Just White
                     _ -> Nothing
parseCommand _ = Nothing

evalCommand :: BotState -> Command -> IO ([String], BotState)
evalCommand s@(BotState _ whites blacks _) Players = return (players, s)
    where players = ["White: " ++ unwords whites, "Black: " ++ unwords blacks]
--evalCommand s (AddPlayer White nick) = undefined
--evalCommand s (AddPlayer Black nick) = undefined
--evalCommand s (RemovePlayer White nick) = undefined
--evalCommand s (RemovePlayer Black nick) = undefined
--evalCommand s Undo = undefined
evalCommand s PrintFEN = return ([writeFEN (botStateGame s)], s)
evalCommand s Help = return (helpText, s)
evalCommand s@(BotState game _ _ _) PrintUnicodeBoard = return (lines (printBoardUnicode (board game)), s)
evalCommand s@(BotState game _ _ _) PrintCompactBoard = return (lines (printBoardCompact (board game)), s)
evalCommand s@(BotState game _ _ _) PrintBoard = return (lines (printBoard (board game)), s)
evalCommand s@(BotState game _ _ _) PrintStatus = return (lines (printColoredState game), s)
evalCommand s@(BotState game whites blacks cmds) cmd@(MakeMove moveStr)
    = case game' of
        Just newGameState -> return ([moved, winnerStatus newGameState], BotState newGameState whites blacks (cmd:cmds))
        Nothing -> return (["Invalid move"], s)
    where game' = move game moveStr
          moved = show (currentPlayer game) ++ " player moved: " ++ moveStr ++ ", new FEN: " ++ writeFEN (fromJust game')
          winnerStatus g = if winner g /= Nothing
                                   then "Game over. The winner is " ++ show (currentPlayer game)
                                   else ""

evalCommand _ NewGame = return (["Game restarted"], initialBotState)
evalCommand s@(BotState _ _ _ _) ClaimDraw = return (["Claiming draw is not implemented"], s)
evalCommand s Invalid = return (["Invalid command"], s)
evalCommand s _ = return (["Invalid command"], s)

helpText :: [String]
helpText = ["Available commands:"
           , "!move MOVE    Makes a move. Move is given in coordinate notation. Example: \"!move b1-c3\"."
           , "!newgame      Starts a new game."
           , "!board        Prints the current game board."
           , "!uboard       Prints the current game board using unicode UTF-8 symbols."
           , "!cboard       Prints the current game board using compact notation."
           , "!status       Prints the current game status."
           , "!fen          Prints out the FEN notation of the current game status."]

serialize :: BotState -> String -> IO ()
serialize = undefined

deserialize :: String -> IO (Maybe BotState)
deserialize = undefined

runBot :: Handle -> BotState -> IO BotState
runBot h state = do s <- readChannel h
                    let c = parseCommand s
                    case c of
                      Nothing -> runBot h state
                      Just cmd -> do (output, state') <- evalCommand state cmd
                                     mapM_ (writeChannel (length output > 8) h) output
                                     runBot h state'

main :: IO ()
main = do h <- initialize
          runBot h initialBotState >> return ()
