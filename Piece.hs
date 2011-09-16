module Piece (Piece(..), Color(..), Type(..), Castling(..),
              printPiece, printBigPiece, moveDirections, moveSquares, attackSquares, opponent, parsePiece,
              getEnPassantTargetSquare, getCastling, getCastlingSquares, isPromotionSquare, pieceTypeString, isDoubleMove,
              getInvalidatedCastlings, getCastlings) where

import Data.Maybe
import Data.Char
import Data.List

data Piece = Piece Type Color
             deriving (Eq, Show, Read)

data Color = White | Black
             deriving (Eq, Show, Read)

data Type = Pawn | Knight | Bishop | Rook | Queen | King
            deriving (Eq, Show, Read)

data Castling = Long Color | Short Color
                deriving (Eq, Show, Read)

pieceChars :: [(Type, Char)]
pieceChars = [(Pawn, 'P'), (Knight, 'N'), (Bishop, 'B'), (Rook, 'R'), (Queen, 'Q'), (King, 'K')]

opponent :: Color -> Color
opponent White = Black
opponent Black = White

pieceTypeString :: Type -> String
pieceTypeString Pawn = ""
pieceTypeString t = [fromJust $ lookup t pieceChars]

printPiece :: Piece -> Char
printPiece (Piece t color) = case color of
                               White -> toUpper c
                               Black -> toLower c
    where (Just c) = lookup t pieceChars

printBigPiece :: Piece -> String
printBigPiece p@(Piece _ color) = case color of
                                  White -> toUpper (printPiece p) : "W"
                                  Black -> toUpper (printPiece p) : "B"

parsePiece :: Char -> Maybe Piece
parsePiece c = do
  pieceType <- rlookup (toUpper c) pieceChars
  if isUpper c then return (Piece pieceType White) else return (Piece pieceType Black)
    where rlookup x = lookup x . map swap
          swap (x, y) = (y, x)

diagonal :: [(Int, Int)]
diagonal = [(-1, -1), (-1, 1), (1, 1), (1, -1)]

perpendicular :: [(Int, Int)]
perpendicular = [(-1, 0), (0, 1), (1, 0), (0, -1)]

moveDirections :: Piece -> [(Int, Int)]
moveDirections (Piece Pawn White) = [(-1, 0)]
moveDirections (Piece Pawn Black) = [(1, 0)]
moveDirections (Piece Knight _) = [(i, j) | i <- [-2..2], j <- [-2..2], (i /= 0 || j /= 0) && not (abs (i - j) == 0 || abs (i + j) == 0 || i == 0 || j == 0)]
moveDirections (Piece Bishop _) = diagonal
moveDirections (Piece Rook _) = perpendicular
moveDirections (Piece Queen _) = perpendicular ++ diagonal
moveDirections (Piece King _) = perpendicular ++ diagonal

moveSquaresDiff :: Piece -> [(Int, Int)]
moveSquaresDiff (Piece King _) = [(i, j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]
moveSquaresDiff (Piece Queen _) = [(i, j) | i <- [-8..8], j <- [-8..8], (i /= 0 || j /= 0) && (abs (i - j) == 0 || abs (i + j) == 0 || i == 0 || j == 0)]
moveSquaresDiff (Piece Rook _) = [(i, j) | i <- [-8..8], j <- [-8..8], (i /= 0 || j /= 0) && (i == 0 || j == 0)]
moveSquaresDiff (Piece Bishop _) = [(i, j) | i <- [-8..8], j <- [-8..8], (i /= 0 || j /= 0) && (abs (i - j) == 0 || abs (i + j) == 0)]
moveSquaresDiff (Piece Knight _) = [(i, j) | i <- [-2..2], j <- [-2..2], (i /= 0 || j /= 0) && not (abs (i - j) == 0 || abs (i + j) == 0 || i == 0 || j == 0)]
moveSquaresDiff (Piece Pawn White) = [(-1, 0)]
moveSquaresDiff (Piece Pawn Black) = [(1, 0)]

attackSquaresDiff :: Piece -> [(Int, Int)]
attackSquaresDiff (Piece Pawn White) = [(-1, -1), (-1, 1)]
attackSquaresDiff (Piece Pawn Black) = [(1, -1), (1, 1)]
attackSquaresDiff piece = moveSquaresDiff piece

fromDiff :: (Piece -> [(Int, Int)]) -> (Int, Int) -> Piece -> [(Int, Int)]
fromDiff f (cx, cy) = filter (\(x, y) -> x >= 0 && y >= 0 && x <= 7 && y <= 7) . map (\(x, y) -> (x + cx, y + cy)) . f

moveSquares :: (Int, Int) -> Piece -> [(Int, Int)]
moveSquares (6, y) (Piece Pawn White) = [(5, y), (4, y)]
moveSquares (1, y) (Piece Pawn Black) = [(2, y), (3, y)]
moveSquares square piece = fromDiff moveSquaresDiff square piece

attackSquares :: (Int, Int) -> Piece -> [(Int, Int)]
attackSquares = fromDiff attackSquaresDiff

getEnPassantTargetSquare :: (Int, Int) -> Color -> (Int, Int)
getEnPassantTargetSquare (x, y) White = (x - 1, y)
getEnPassantTargetSquare (x, y) Black = (x + 1, y)

getCastling :: Color -> (Int, Int) -> (Int, Int) -> Maybe Castling
getCastling White (7, 4) (7, 0) = Just (Long White)
getCastling White (7, 4) (7, 7) = Just (Short White)
getCastling Black (0, 4) (0, 0) = Just (Long Black)
getCastling Black (0, 4) (0, 7) = Just (Short Black)
getCastling _ _ _ = Nothing

getCastlings :: Color -> [Castling]
getCastlings color = [Long color, Short color]

getInvalidatedCastlings :: Piece -> (Int, Int) -> [Castling]
getInvalidatedCastlings (Piece King color) _ = [Long color, Short color]
getInvalidatedCastlings (Piece Rook White) (7, 0) = [Long White]
getInvalidatedCastlings (Piece Rook White) (7, 7) = [Short White]
getInvalidatedCastlings (Piece Rook Black) (0, 0) = [Long Black]
getInvalidatedCastlings (Piece Rook Black) (0, 7) = [Short Black]
getInvalidatedCastlings _ _ = []

getCastlingSquares :: Castling -> [(Int, Int)]
getCastlingSquares (Long White) = [(7, y) | y <- [0..4]]
getCastlingSquares (Short White) = [(7, y) | y <- [4..7]]
getCastlingSquares (Long Black) = [(0, y) | y <- [0..4]]
getCastlingSquares (Short Black) = [(0, y) | y <- [4..7]]

isPromotionSquare :: (Int, Int) -> Color -> Bool
isPromotionSquare (0, _) White = True
isPromotionSquare (7, _) Black = True
isPromotionSquare _ _ = False

isDoubleMove :: (Int, Int) -> (Int, Int) -> Color -> Bool
isDoubleMove (6, _) (4, _) White = True
isDoubleMove (1, _) (3, _) Black = True
isDoubleMove _ _ _ = False
