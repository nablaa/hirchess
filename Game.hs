module Game (GameState, initialState, applyMove, printColoredState) where

import Data.List
import Board
import Piece
import Move
import FEN
import Colors

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
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) castlings (Just $ fromEnPassantTargetSquare end) halfmove moves)) move

getInvalidatedCastlings :: Piece -> Coordinates -> [Castling]
getInvalidatedCastlings (Piece King color) _ = [Long color, Short color]
getInvalidatedCastlings (Piece Rook White) (7, 0) = [Long White]
getInvalidatedCastlings (Piece Rook White) (7, 7) = [Short White]
getInvalidatedCastlings (Piece Rook Black) (0, 0) = [Long Black]
getInvalidatedCastlings (Piece Rook Black) (0, 7) = [Short Black]
getInvalidatedCastlings _ _ = []

incrMoves :: GameState -> GameState
incrMoves state | player state == White = state { moveNumber = moveNumber state + 1 }
                | otherwise = state

setHalfMoves :: GameState -> Move -> GameState
setHalfMoves state (Move _ (Piece Pawn _) _ _) = state { halfmoveClock = 0 }
setHalfMoves state _ = state { halfmoveClock = halfmoveClock state + 1 }

printColoredState :: GameState -> String
printColoredState state = printBigPrettyColoredBoard (board state) ++ "\n\n"
                          ++ "Current player: " ++ withColor (playerColor color) (show color)
                          ++ "\t\tMove number: " ++ show (moveNumber state) ++ "\n"
                          ++ "FEN: " ++ writeFEN state
    where color = player state
          playerColor White = whitePlayerColor
          playerColor Black = blackPlayerColor
