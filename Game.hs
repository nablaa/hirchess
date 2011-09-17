module Game (GameState, initialState, applyMove, debugPrintState) where

import Data.List
import Board
import Piece
import Move
import FEN

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
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) (castlings \\ getInvalidatedCastlings piece start) Nothing halfmove moves)) move
applyMove (State board player castlings enpassant halfmove moves) move@(Move Capture piece start _)
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) (castlings \\ getInvalidatedCastlings piece start) Nothing halfmove moves)) move
applyMove (State board player castlings enpassant halfmove moves) move@(Move (Castling _) _ _ _)
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) (castlings \\ getCastlings player) Nothing halfmove moves)) move
applyMove (State board player castlings enpassant halfmove moves) move@(Move (EnPassant _) _ _ _)
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) castlings Nothing halfmove moves)) move
applyMove (State board player castlings enpassant halfmove moves) move@(Move (Promotion _) _ _ _)
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) castlings Nothing halfmove moves)) move
applyMove (State board player castlings enpassant halfmove moves) move@(Move (PawnDoubleMove) _ _ end)
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) castlings (Just end) halfmove moves)) move

incrMoves :: GameState -> GameState
incrMoves state | player state == White = state { moveNumber = moveNumber state + 1 }
                | otherwise = state

setHalfMoves :: GameState -> Move -> GameState
setHalfMoves state (Move _ (Piece Pawn _) _ _) = state { halfmoveClock = 0 }
setHalfMoves state _ = state { halfmoveClock = halfmoveClock state + 1 }

debugPrintState :: GameState -> IO ()
debugPrintState state = putStrLn $ printBigPrettyBoard (board state) ++ "\n\n"
                        ++ "Current player: " ++ show (player state) ++ "\t\tMove number: " ++ show (moveNumber state) ++ "\n"
                        ++ "FEN: " ++ writeFEN state
