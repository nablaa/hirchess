module FEN (writeFEN, readFEN) where

import Data.List
import Move
import Board
import Piece

writeBoard :: Board -> String
writeBoard = intercalate "/" . lines . concat . map emptyToNum . group . printBoard
    where emptyToNum str@(' ':_) = show $ length str
          emptyToNum str = str

writePlayer :: Color -> String
writePlayer White = "w"
writePlayer Black = "b"

writeCastlings :: [Castling] -> String
writeCastlings = sort . concat . map toString
    where toString (Long White) = "Q"
          toString (Short White) = "K"
          toString (Long Black) = "q"
          toString (Short Black) = "k"

writeEnPassant :: Maybe Coordinates -> String
writeEnPassant Nothing = "-"
writeEnPassant (Just square) = coordinatesToString square

writeFEN :: GameState -> String
writeFEN state = unwords [writeBoard (board state), writePlayer (player state), writeCastlings (castlingsPossible state), writeEnPassant (enPassantSquare state), show (halfmoveClock state), show (moveNumber state)]

readFEN :: String -> Maybe GameState
readFEN = undefined
