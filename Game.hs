module Game (GameState, initialState) where

import Board
import Piece
import Move

initialState :: GameState
initialState = State initialBoard White initialCastlings Nothing 0 1
    where initialCastlings = [Long White, Short White, Long Black, Short Black]

hasEnded :: GameState -> Bool
hasEnded = undefined

getWinner :: GameState -> Maybe Color
getWinner = undefined

isDraw :: GameState -> Bool
isDraw = undefined

applyMove :: GameState -> Move -> GameState
applyMove = undefined

debugPrintState :: GameState -> IO ()
debugPrintState = undefined
