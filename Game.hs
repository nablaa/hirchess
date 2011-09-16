module Game (GameState, initialState) where

import Data.List
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
applyMove (State board player castlings enpassant halfmove moves) move@(Move Movement piece start _)
    = incrMoves (State (applyMoveBoard board move) (opponent player) (castlings \\ getInvalidatedCastlings piece start) Nothing (halfmove + 1) moves)
applyMove (State board player castlings enpassant halfmove moves) move@(Move Capture piece start _)
    = incrMoves (State (applyMoveBoard board move) (opponent player) (castlings \\ getInvalidatedCastlings piece start) Nothing 0 moves)
applyMove (State board player castlings enpassant halfmove moves) move@(Move (Castling _) _ _ _)
    = incrMoves (State (applyMoveBoard board move) (opponent player) (castlings \\ getCastlings player) Nothing (halfmove + 1) moves)
applyMove (State board player castlings enpassant halfmove moves) move@(Move (EnPassant _) _ _ _)
    = incrMoves (State (applyMoveBoard board move) (opponent player) castlings Nothing 0 moves)
applyMove (State board player castlings enpassant halfmove moves) move@(Move (Promotion _) _ _ _)
    = incrMoves (State (applyMoveBoard board move) (opponent player) castlings Nothing 0 moves)
applyMove (State board player castlings enpassant halfmove moves) move@(Move (PawnDoubleMove) _ _ end)
    = incrMoves (State (applyMoveBoard board move) (opponent player) castlings (Just end) (halfmove + 1) moves)

incrMoves :: GameState -> GameState
incrMoves state | player state == Black = state { moveNumber = moveNumber state + 1 }
                | otherwise = state


debugPrintState :: GameState -> IO ()
debugPrintState = undefined
