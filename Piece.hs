module Piece (Piece(..), Color(..), Type(..),
              printPiece, printBigPiece, printBigPieceColored, parsePiece,
              moveDirections, moveSquares, attackSquares, opponent) where

import Data.Maybe
import Data.Char
import Data.List
import Colors

data Piece = Piece Type Color
             deriving (Eq, Show, Read)

data Color = White | Black
             deriving (Eq, Show, Read)

data Type = Pawn | Knight | Bishop | Rook | Queen | King
            deriving (Eq, Show, Read)

pieceChars :: [(Type, Char)]
pieceChars = [(Pawn, 'P'), (Knight, 'N'), (Bishop, 'B'), (Rook, 'R'), (Queen, 'Q'), (King, 'K')]

opponent :: Color -> Color
opponent White = Black
opponent Black = White

printPiece :: Piece -> Char
printPiece (Piece t color) = case color of
                               White -> toUpper c
                               Black -> toLower c
    where (Just c) = lookup t pieceChars

printBigPiece :: Piece -> String
printBigPiece p@(Piece _ color) = toUpper (printPiece p) : case color of
                                  White -> "W"
                                  Black -> "B"

printBigPieceColored :: Piece -> String
printBigPieceColored p@(Piece _ color) = withColor (printColor color) [printPiece p, ' ']
    where printColor White = whitePlayerColor
          printColor Black = blackPlayerColor

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

