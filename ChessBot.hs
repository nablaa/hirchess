module ChessBot () where

import Network
import System.IO
import Text.Printf
import Data.List
import Data.Char
import System.Exit
import IRC
import Game
import Piece
import Move

data Command = Players | AddPlayer Color String | RemovePlayer Color String
             | PrintBoard | PrintFEN | PrintStatus
             | MakeMove String | Undo | NewGame | ClaimDraw | Invalid
               deriving (Eq, Show, Read)

data BotState = BotState {
      game :: GameState
    , whitePlayers :: [String]
    , blackPlayers :: [String]
    , commandHistory :: [Command]
    } deriving (Eq, Show, Read)

initialBotState :: BotState
initialBotState = BotState initialState [] [] []

parseCommand :: String -> Maybe Command
parseCommand str | cmd "!players" = Just Players
                 | cmd "!player" && count >= 3 && arg 1 == "add" = case player of
                                                                 Just p -> Just $ AddPlayer p (arg 3)
                                                                 Nothing -> Nothing
                 | cmd "!player" && count >= 3 && arg 1 == "remove" = case player of
                                                                 Just p -> Just $ AddPlayer p (arg 3)
                                                                 Nothing -> Nothing
                 | cmd "!board" = Just PrintBoard
                 | cmd "!fen" = Just PrintFEN
                 | cmd "!status" = Just PrintStatus
                 | cmd "!move" && count >= 2 && arg 1 == "draw" = Just ClaimDraw
                 | cmd "!move" && count >= 2 = Just $ MakeMove $ arg 1
                 | cmd "!undo" = Just Undo
                 | cmd "!newgame" = Just NewGame
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
evalCommand s (AddPlayer White nick) = undefined
evalCommand s (AddPlayer Black nick) = undefined
evalCommand s (RemovePlayer White nick) = undefined
evalCommand s (RemovePlayer Black nick) = undefined
evalCommand s PrintBoard = undefined
evalCommand s PrintFEN = undefined
evalCommand s PrintStatus = undefined
evalCommand s (MakeMove move) = undefined
evalCommand s Undo = undefined
evalCommand s NewGame = undefined
evalCommand s ClaimDraw = undefined
evalCommand s Invalid = return (["Invalid command"], s)

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
                                     mapM_ (writeChannel h) output
                                     runBot h state'

main :: IO ()
main = do h <- initialize
          runBot h initialBotState >> return ()
