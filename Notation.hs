module Notation (NotationParser, NotationPrinter,
                 printLongAlgebraicNotation,
                 printAlgebraicNotation,
                 printCoordinateNotation, parseCoordinateNotation,
                 printMoveList, printMoveListColumns) where

import Move
import Piece
import Board
import Data.Char

type NotationParser = GameState -> String -> Maybe (Coordinates, Coordinates)
type NotationPrinter = Move -> String

parseMove :: GameState -> String -> Maybe (Coordinates, Coordinates)
parseMove state str = undefined

longAlgebraicNotation' :: Move -> String -> String
longAlgebraicNotation' (Move _ (Piece pieceType _) start end) separator = pieceStr ++ startStr ++ separator ++ endStr
    where pieceStr = case pieceType of
                       Pawn -> ""
                       _ -> printPiece (Piece pieceType White)
          startStr = coordinatesToString start
          endStr = coordinatesToString end

printLongAlgebraicNotation :: NotationPrinter
printLongAlgebraicNotation move@(Move Movement _ _ _) = longAlgebraicNotation' move "-"
printLongAlgebraicNotation move@(Move Capture _ _ _) = longAlgebraicNotation' move "x"
printLongAlgebraicNotation (Move (Castling (Long _)) _ _ _) = "O-O-O"
printLongAlgebraicNotation (Move (Castling (Short _)) _ _ _) = "O-O"
printLongAlgebraicNotation move@(Move (EnPassant _) _ _ _) = longAlgebraicNotation' move "x"
printLongAlgebraicNotation move@(Move (Promotion (Piece promoted _)) _ _ _) = longAlgebraicNotation' move "-" ++ printPiece (Piece promoted White)
printLongAlgebraicNotation move@(Move PawnDoubleMove _ _ _) = longAlgebraicNotation' move "-"

printAlgebraicNotation :: NotationPrinter
printAlgebraicNotation (Move Movement (Piece Pawn _) _ end) = coordinatesToString end
printAlgebraicNotation (Move PawnDoubleMove (Piece Pawn _) _ end) = coordinatesToString end
printAlgebraicNotation (Move Capture (Piece Pawn _) start end) = head (coordinatesToString start) : "x" ++ coordinatesToString end
printAlgebraicNotation (Move (EnPassant _) _ start end) = head (coordinatesToString start) : "x" ++ coordinatesToString end ++ "e.p"
printAlgebraicNotation (Move Movement piece _ end) = map toUpper (printPiece piece) ++ coordinatesToString end
printAlgebraicNotation (Move Capture piece _ end) = map toUpper (printPiece piece) ++ "x" ++ coordinatesToString end
printAlgebraicNotation (Move (Castling (Long _)) _ _ _) = "O-O-O"
printAlgebraicNotation (Move (Castling (Short _)) _ _ _) = "O-O"
printAlgebraicNotation (Move (Promotion (Piece promoted _)) _ _ end) = coordinatesToString end ++ printPiece (Piece promoted White)

printCoordinateNotation :: NotationPrinter
printCoordinateNotation (Move _ _ coord1 coord2) = map toUpper $ coordinatesToString coord1 ++ "-" ++ coordinatesToString coord2

parseCoordinateNotation :: NotationParser
parseCoordinateNotation _ (c1:r1:'-':c2:r2:[]) = do coord1 <- coord1'
                                                    coord2 <- coord2'
                                                    Just (coord1, coord2)
    where coord1' = stringToCoordinates $ map toLower [c1, r1]
          coord2' = stringToCoordinates $ map toLower [c2, r2]
parseCoordinateNotation _ _ = Nothing

printMoveList :: NotationPrinter -> [Move] -> String
printMoveList f = unlines . map f

printMoveListColumns :: NotationPrinter -> [Move] -> String
printMoveListColumns _ [] = []
printMoveListColumns f [move] = f move
printMoveListColumns f (white:black:rest) = whiteStr ++ padding ++ f black ++ "\n" ++ printMoveListColumns f rest
    where whiteStr = f white
          padding = replicate (8 - length whiteStr) ' '
