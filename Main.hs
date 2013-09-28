module Main (main) where

import Game
import Move
import Colors
import Notation

main :: IO ()
main = do _ <- debugPlayGame initialState
          putStrLn ""

debugPlayGame :: GameState -> IO (GameState)
debugPlayGame state | getWinner state /= Nothing = putStrLn (printColoredState state) >> return state
                    | otherwise = do putStrLn $ printColoredState state
                                     putStrLn movePrompt
                                     moveStr <- getLine
                                     let coords = parseCoordinateNotation state moveStr
                                     case coords of
                                       Just (coord1, coord2) -> case getMove state coord1 coord2 of
                                                                 Just move -> if isLegalMove state move then
                                                                                 debugPlayGame $ applyMove state move
                                                                             else
                                                                                 putStrLn illegalMove >> debugPlayGame state
                                                                 Nothing -> putStrLn invalidMove >> debugPlayGame state
                                       Nothing -> if moveStr == "draw" && canClaimDraw state then
                                                     putStrLn draw >> return state
                                                 else
                                                     putStrLn invalidCoordinates >> debugPlayGame state
    where movePrompt = "\n" ++ withColor promptColor "Enter move: "
          illegalMove = withColor errorColor "Illegal move" ++ "\n\n"
          invalidMove = withColor errorColor "Invalid move" ++ "\n\n"
          invalidCoordinates = withColor errorColor "Invalid coordinates" ++ "\n\n"
          draw = "\n" ++ withColor notificationColor "Game over. The game is a draw!"
