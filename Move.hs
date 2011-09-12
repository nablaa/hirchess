module Move (GameState, Move) where

import Data.List
import Board
import Piece

data Move = Move MoveType Piece Coordinates Coordinates
data MoveType = Movement | Capture | Castling | EnPassant | Promotion Type | PawnDoubleMove

data GameState = State {
      board :: Board
    , player :: Color
    , castlingsPossible :: [Castling]
    , enPassantSquare :: Maybe Coordinates
    , halfmoveClock :: Integer
    , moveNumber :: Integer
    } deriving (Eq, Show, Read)


canCastle :: GameState -> Coordinates -> Coordinates -> Bool
canCastle (State _ _ [] _ _ _) _ _ = False
canCastle (State board player castlings _ _ _) start end
    | startPiece /= Just (Piece King player) = False
    | endPiece /= Just (Piece Rook player) = False
    | castling' == Nothing = False
    | not $ castling `elem` castlings = False
    | any (isChecked board player) squares = False
    | not $ all (isEmpty board) (squares \\ [start, end]) = False
    where startPiece = getPiece board start
          endPiece = getPiece board end
          (Just king) = startPiece
          (Just rook) = endPiece
          castling' = getCastling player start end
          (Just castling) = castling'
          squares = getCastlingSquares castling
canCastle _ _ _ = True

canEnPassant :: GameState -> Coordinates -> Coordinates -> Bool
canEnPassant (State _ _ _ Nothing _ _) _ _ = False
canEnPassant (State board player _ (Just square) _ _) start end
    | startPiece /= Just (Piece Pawn player) = False
    | endPiece /= Nothing = False
    | not $ end `elem` attackSquares start piece = False
    | targetSquare /= square = False
    | targetPiece /= Just (Piece Pawn (opponent player)) = False
    where startPiece = getPiece board start
          (Just piece) = startPiece
          endPiece = getPiece board end
          targetSquare = getEnPassantTargetSquare end player
          targetPiece = getPiece board targetSquare
canEnPassant _ _ _ = True
