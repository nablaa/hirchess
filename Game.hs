module Game (initialState, applyMove, printColoredState, getWinner, canClaimDraw) where

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

getWinner :: GameState -> Maybe Color
getWinner state | null (getAllLegalMoves state) = Just $ opponent $ player state
                | otherwise = Nothing

canClaimDraw :: GameState -> Bool
canClaimDraw state = halfmoveClock state >= 50 || identicalBoardCount state >= 3

identicalBoardCount :: GameState -> Int
identicalBoardCount state |length moves >= 1 && b == (board state) = length $ filter (==b) boards
    where boards = map board $ collectStates initialState moves
          b = last boards
          moves = reverse $ moveHistory state
identicalBoardCount _ = 0

collectStates :: GameState -> [Move] -> [GameState]
collectStates state moves = scanl applyMove state moves

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
                          ++ "              Move: " ++ show (moveNumber state) ++ "\n"
                          ++ "FEN: " ++ writeFEN state
                          ++ drawStatus
                          ++ winnerStatus) (printMoveHistory state ++ unlines (repeat "\n"))
    where color = player state
          playerColor White = whitePlayerColor
          playerColor Black = blackPlayerColor
          drawStatus = if canClaimDraw state then
                           "\n" ++ withColor notificationColor "The player can claim draw"
                       else
                           ""
          winnerStatus = case getWinner state of
                           Just player -> "\n" ++ withColor notificationColor "Game over. The winner is " ++ withColor (playerColor player) (show player)
                           Nothing -> ""

printMoveHistory :: GameState -> String
printMoveHistory state = join ". " numbers list
    where list = printMoveListColumns printLongAlgebraicNotation (reverse (moveHistory state))
          numbers = unlines [show x | x <- [1..]]

join :: String ->  String -> String -> String
join sep str1 str2 = unlines $ zipWith (\x y -> x ++ sep ++ y) (lines str1) (lines str2)
