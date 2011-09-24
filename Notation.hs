module Notation (NotationParser, NotationPrinter, longAlgebraicNotation, parseCoordinateNotation, printMoveList, printMoveListColumns) where

import Move
import Piece
import Board

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

longAlgebraicNotation :: NotationPrinter
longAlgebraicNotation move@(Move Movement _ _ _) = longAlgebraicNotation' move "-"
longAlgebraicNotation move@(Move Capture _ _ _) = longAlgebraicNotation' move "x"
longAlgebraicNotation (Move (Castling (Long _)) _ _ _) = "O-O-O"
longAlgebraicNotation (Move (Castling (Short _)) _ _ _) = "O-O"
longAlgebraicNotation move@(Move (EnPassant _) _ _ _) = longAlgebraicNotation' move "x"
longAlgebraicNotation move@(Move (Promotion (Piece promoted _)) _ _ _) = longAlgebraicNotation' move "-" ++ printPiece (Piece promoted White)
longAlgebraicNotation move@(Move PawnDoubleMove _ _ _) = longAlgebraicNotation' move "-"

parseCoordinateNotation :: NotationParser
parseCoordinateNotation _ (c1:r1:'-':c2:r2:[]) = do coord1 <- coord1'
                                                    coord2 <- coord2'
                                                    Just (coord1, coord2)
    where coord1' = stringToCoordinates [c1, r1]
          coord2' = stringToCoordinates [c2, r2]
parseCoordinateNotation _ _ = Nothing

printMoveList :: NotationPrinter -> [Move] -> String
printMoveList f = unlines . map f

printMoveListColumns :: NotationPrinter -> [Move] -> String
printMoveListColumns _ [] = []
printMoveListColumns f [move] = f move
printMoveListColumns f (white:black:rest) = whiteStr ++ padding ++ f black ++ "\n" ++ printMoveListColumns f rest
    where whiteStr = f white
          padding = replicate (8 - length whiteStr) ' '