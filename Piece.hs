module Piece (Piece(..), Color(..), Type(..),
              printPiece, printBigPiece, printBigPieceColored, parsePiece, opponent,
              movePattern, capturePattern) where

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

diagonal :: [(Int, Int)]
diagonal = [(-1, -1), (-1, 1), (1, 1), (1, -1)]

perpendicular :: [(Int, Int)]
perpendicular = [(-1, 0), (0, 1), (1, 0), (0, -1)]

movePattern :: Piece -> [(Int, Int)]
movePattern (Piece Pawn White) = [(-1, 0)]
movePattern (Piece Pawn Black) = [(1, 0)]
movePattern (Piece Knight _) = [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
movePattern (Piece Bishop _) = diagonal
movePattern (Piece Rook _) = perpendicular
movePattern (Piece Queen _) = perpendicular ++ diagonal
movePattern (Piece King _) = perpendicular ++ diagonal

capturePattern :: Piece -> [(Int, Int)]
capturePattern (Piece Pawn White) = [(-1, -1), (-1, 1)]
capturePattern (Piece Pawn Black) = [(1, -1), (1, 1)]
capturePattern piece = movePattern piece

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
