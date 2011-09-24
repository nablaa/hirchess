module UI () where

import Game
import Move
import Colors

main :: IO ()
main = do _ <- debugPlayGame initialState
          putStrLn ""

debugPlayGame :: GameState -> IO (GameState)
debugPlayGame state | hasEnded state = do putStrLn $ printColoredState state
                                          return state
                    | otherwise = do putStrLn $ printColoredState state
                                     putStrLn movePrompt
                                     moveStr <- getLine
                                     let coords = parseLongAlgebraicNotation moveStr
                                     case coords of
                                       Just (coord1, coord2) -> case getMove state coord1 coord2 of
                                                                 Just move -> if isLegalMove state move then
                                                                                 debugPlayGame $ applyMove state move
                                                                             else
                                                                                 putStrLn illegalMove >> debugPlayGame state
                                                                 Nothing -> putStrLn invalidMove >> debugPlayGame state
                                       Nothing -> putStrLn invalidCoordinates >> debugPlayGame state
    where movePrompt = withColor promptColor "\nEnter move: "
          illegalMove = withColor errorColor "Illegal move\n\n"
          invalidMove = withColor errorColor "Invalid move\n\n"
          invalidCoordinates = withColor errorColor "Invalid coordinates\n\n"
