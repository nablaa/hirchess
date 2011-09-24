module Game (initialState, applyMove, printColoredState, hasEnded) where

import Data.List
import Board
import Piece
import Move
import FEN
import Colors
import Notation

initialState :: GameState
initialState = State initialBoard White initialCastlings Nothing 0 1 []
    where initialCastlings = [Long White, Short White, Long Black, Short Black]

hasEnded :: GameState -> Bool
hasEnded state = null (getAllLegalMoves state) || isDraw state

getWinner :: GameState -> Maybe Color
getWinner state | null (getAllLegalMoves state) = Just $ opponent $ player state
                | otherwise = Nothing

isDraw :: GameState -> Bool
isDraw state = False

applyMove :: GameState -> Move -> GameState
applyMove (State board player castlings enpassant halfmove moves history) move@(Move Movement piece start _)
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) (castlings \\ getInvalidatedCastlings piece start) Nothing halfmove moves (move:history))) move
applyMove (State board player castlings enpassant halfmove moves history) move@(Move Capture piece start _)
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) (castlings \\ getInvalidatedCastlings piece start) Nothing halfmove moves (move:history))) move
applyMove (State board player castlings enpassant halfmove moves history) move@(Move (Castling _) _ _ _)
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) (castlings \\ getCastlings player) Nothing halfmove moves (move:history))) move
applyMove (State board player castlings enpassant halfmove moves history) move@(Move (EnPassant _) _ _ _)
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) castlings Nothing halfmove moves (move:history))) move
applyMove (State board player castlings enpassant halfmove moves history) move@(Move (Promotion _) _ _ _)
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) castlings Nothing halfmove moves (move:history))) move
applyMove (State board player castlings enpassant halfmove moves history) move@(Move (PawnDoubleMove) _ _ end)
    = setHalfMoves (incrMoves (State (applyMoveBoard board move) (opponent player) castlings (Just $ fromEnPassantTargetSquare end) halfmove moves (move:history))) move

getInvalidatedCastlings :: Piece -> Coordinates -> [Castling]
getInvalidatedCastlings (Piece King color) _ = [Long color, Short color]
getInvalidatedCastlings (Piece Rook White) (7, 0) = [Long White]
getInvalidatedCastlings (Piece Rook White) (7, 7) = [Short White]
getInvalidatedCastlings (Piece Rook Black) (0, 0) = [Long Black]
getInvalidatedCastlings (Piece Rook Black) (0, 7) = [Short Black]
getInvalidatedCastlings _ _ = []

getCastlings :: Color -> [Castling]
getCastlings color = [Long color, Short color]

incrMoves :: GameState -> GameState
incrMoves state | player state == White = state { moveNumber = moveNumber state + 1 }
                | otherwise = state

setHalfMoves :: GameState -> Move -> GameState
setHalfMoves state (Move _ (Piece Pawn _) _ _) = state { halfmoveClock = 0 }
setHalfMoves state _ = state { halfmoveClock = halfmoveClock state + 1 }

printColoredState :: GameState -> String
printColoredState state = join "    " (printBoardColored (board state) ++ "\n\n"
                          ++ "Player: " ++ withColor (playerColor color) (show color)
                          ++ "\t\tMove: " ++ show (moveNumber state) ++ "\n"
                          ++ "FEN: " ++ writeFEN state
                          ++ endStatus) (printMoveHistory state ++ unlines (repeat "\n"))
    where color = player state
          playerColor White = whitePlayerColor
          playerColor Black = blackPlayerColor
          endStatus = if hasEnded state then
                          "\n" ++ withColor notificationColor "Game over: " ++ drawStatus ++ winnerStatus ++ "\n"
                      else
                          ""
          drawStatus = if isDraw state then
                           withColor notificationColor "The game is a draw"
                       else
                           ""
          winnerStatus = case getWinner state of
                           Just player -> withColor notificationColor "The winner is: " ++ withColor (playerColor player) (show player)
                           Nothing -> ""

printMoveHistory :: GameState -> String
printMoveHistory state = join ". " numbers list
    where list = printMoveListColumns (reverse (moveHistory state))
          numbers = unlines [show x | x <- [1..]]


join :: String ->  String -> String -> String
join sep str1 str2 = unlines $ zipWith (\x y -> x ++ sep ++ y) (lines str1) (lines str2)
