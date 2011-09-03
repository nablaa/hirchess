module Board (Board, Coordinates) where

import Data.Maybe
import Data.Array
import Piece

data Square = Square Piece | Empty
              deriving (Eq, Show, Read)

type Coordinates = (Int, Int)
type Board = Array Coordinates Square


initialBoard :: Board
initialBoard = undefined

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
printBoard = undefined

printPrettyBoard :: Board -> String
printPrettyBoard = undefined

printBigPrettyBoard :: Board -> String
printBigPrettyBoard = undefined

