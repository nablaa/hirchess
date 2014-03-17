module Main (main) where

import Chess
import Colors
import UI
import Data.Maybe

main :: IO ()
main = debugPlayGame newGame >> putStrLn ""

debugPlayGame :: GameState -> IO GameState
debugPlayGame state | isJust (winner state) = putStrLn (printColoredState state) >> return state
                    | otherwise = do putStrLn $ printColoredState state
                                     putStrLn movePrompt
                                     moveStr <- getLine
                                     case move state moveStr of
                                             Just newState -> debugPlayGame newState
                                             Nothing -> putStrLn invalidMove >> debugPlayGame state
    where movePrompt = '\n' : withColor promptColor "Enter move: "
          invalidMove = withColor errorColor "Invalid move" ++ "\n\n"
