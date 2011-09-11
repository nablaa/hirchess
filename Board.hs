module Board (Board, Coordinates, initialBoard,
              printBoard, printPrettyBoard, printBigPrettyBoard, printSquares) where

import Data.Char
import Data.Maybe
import Data.Array
import Piece

data Square = Square Piece | Empty
              deriving (Eq, Show, Read)

type Coordinates = (Int, Int)
type Board = Array Coordinates Square


initialBoard :: Board
initialBoard = listArray ((0, 0), (7, 7)) rows
    where officerRow p = map (\x -> Square (Piece x p)) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
          pawnRow p = map (\x -> Square (Piece x p)) $ replicate 8 Pawn
          rows = officerRow Black ++ pawnRow Black ++ replicate 32 Empty ++ pawnRow White ++ officerRow White

emptyBoard :: Board
emptyBoard = listArray ((0, 0), (7, 7)) (repeat Empty)

getPiece :: Board -> Coordinates -> Maybe Piece
getPiece board coordinates | inRange (bounds board) coordinates = f $ board ! coordinates
                           where f Empty = Nothing
                                 f (Square piece) = Just piece
getPiece _ _ = Nothing

getPlayer :: Board -> Coordinates -> Maybe Color
getPlayer b c = case getPiece b c of
                  Just (Piece _ color) -> Just color
                  Nothing -> Nothing

isEmpty :: Board -> Coordinates -> Bool
isEmpty b c = getPiece b c == Nothing

isColor :: Board -> Coordinates -> Color -> Bool
isColor b c color = Just color == getPlayer b c

isInsideBoard :: Coordinates -> Bool
isInsideBoard (i, j) = i >= 0 && i <= 7 && j >= 0 && j <= 7


addPiece :: Board -> Coordinates -> Piece -> Board
addPiece = undefined

removePiece :: Board -> Coordinates -> Board
removePiece = undefined

movePiece :: Board -> Coordinates -> Coordinates -> Board
movePiece = undefined


squareToChar :: Square -> Char
squareToChar Empty = ' '
squareToChar (Square p) = printPiece p

printBoard :: Board -> String
printBoard board = toLines $ foldr f "" (elems board)
    where f sq str = squareToChar sq : str
          toLines [] = []
          toLines str = take 8 str ++ "\n" ++ toLines (drop 8 str)

printPrettyBoard :: Board -> String
printPrettyBoard board = toLines 8 $ foldr f "" (elems board)
    where f sq str = '|' : squareToChar sq : str
          toLines _ [] = line ++ "\n   a b c d e f g h"
          toLines n str = line ++ "\n" ++ [intToDigit n] ++ " " ++ take 16 str ++ "|\n" ++ toLines (n - 1) (drop 16 str)
          line = "  " ++ concat (replicate 8 "+-") ++ "+"

printBigPrettyBoard :: Board -> String
printBigPrettyBoard board = toLines 8 $ foldr f "" (elems board)
    where f sq str = "|" ++ squareToString sq ++ str
          toLines _ [] = line ++ "\n     a    b    c    d    e    f    g    h"
          toLines n str = line ++ "\n" ++ [intToDigit n] ++ "  " ++ take 40 str ++ "|\n" ++ toLines (n - 1) (drop 40 str)
          line = "   " ++ concat (replicate 8 "+----") ++ "+"
          squareToString Empty = "    "
          squareToString (Square p) = " " ++ printBigPiece p ++ " "


printSquares :: (Board -> String) -> [Coordinates] -> String
printSquares f squares = f $ emptyBoard // [(s, Square (Piece Pawn White)) | s <- squares]
