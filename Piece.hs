module Piece (Piece(..), Color(..), Type(..), printPiece) where

import Data.Char

data Piece = Piece Type Color
             deriving (Eq, Show, Read)

data Color = White | Black
             deriving (Eq, Show, Read)

data Type = Pawn | Knight | Bishop | Rook | Queen | King
            deriving (Eq, Show, Read)

pieceChars :: [(Type, Char)]
pieceChars = [(Pawn, 'P'), (Knight, 'N'), (Bishop, 'B'), (Rook, 'R'), (Queen, 'Q'), (King, 'K')]

printPiece :: Piece -> Char
printPiece (Piece t color) = case color of
                               White -> toUpper c
                               Black -> toLower c
    where (Just c) = lookup t pieceChars

