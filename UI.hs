module UI () where

import Game
import Move

main :: IO ()
main = do _ <- debugPlayGame initialState
          putStrLn ""

debugPlayGame :: GameState -> IO (GameState)
debugPlayGame state = do debugPrintState state
                         putStrLn "\nEnter move: "
                         moveStr <- getLine
                         let coords = parseLongAlgebraicNotation moveStr
                         case coords of
                           Just (coord1, coord2) -> case getMove state coord1 coord2 of
                                                      Just move -> if isLegalMove state move then
                                                                      debugPlayGame $ applyMove state move
                                                                  else
                                                                      putStrLn "Illegal move\n\n" >> debugPlayGame state
                                                      Nothing -> putStrLn "Invalid move\n\n" >> debugPlayGame state
                           Nothing -> putStrLn "Invalid coordinates\n\n" >> debugPlayGame state

