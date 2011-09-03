module Board (Board, Coordinates, initialBoard,
              printBoard, printPrettyBoard, printBigPrettyBoard) where

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
emptyBoard = undefined

getPiece :: Board -> Coordinates -> Maybe Piece
getPiece = undefined

getPlayer :: Board -> Coordinates -> Maybe Color
getPlayer = undefined

isEmpty :: Board -> Coordinates -> Bool
isEmpty = undefined

isInsideBoard :: Coordinates -> Bool
isInsideBoard = undefined


addPiece :: Board -> Coordinates -> Piece -> Board
addPiece = undefined

removePiece :: Board -> Coordinates -> Board
removePiece = undefined

movePiece :: Board -> Coordinates -> Coordinates -> Board
movePiece = undefined


printBoard :: Board -> String
printBoard board = toLines $ foldr f "" (elems board)
    where f sq str = squareToChar sq : str
          toLines [] = []
          toLines str = take 8 str ++ "\n" ++ toLines (drop 8 str)
          squareToChar Empty = ' '
          squareToChar (Square p) = printPiece p

printPrettyBoard :: Board -> String
printPrettyBoard = undefined

printBigPrettyBoard :: Board -> String
printBigPrettyBoard = undefined

