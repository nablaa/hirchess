module Board (Board, Coordinates, initialBoard, emptyBoard,
              printBoard, parseBoard, printPrettyBoard, printBigPrettyBoard, printSquares,
              canMove, canCapture, getPiece, getPlayer, isColor, getReachable, addPiece, removePiece, movePiece,
              addPieces, isChecked, isEmpty, debugPrint, coordinatesToString) where

import Data.Char
import Data.Maybe
import Data.Array
import Data.List
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

updateBoard :: Board -> Coordinates -> Square -> Board
updateBoard board coordinates square = board // [(coordinates, square)]

addPiece :: Board -> Coordinates -> Piece -> Board
addPiece board coordinates piece = updateBoard board coordinates (Square piece)

removePiece :: Board -> Coordinates -> Board
removePiece board coordinates = updateBoard board coordinates Empty

movePiece :: Board -> Coordinates -> Coordinates -> Board
movePiece board start end = addPiece board' end piece
    where board' = removePiece board start
          (Just piece) = getPiece board start

addPieces :: Board -> [(Coordinates, Piece)] -> Board
addPieces board list = foldr f board list
    where f (coordinates, piece) b = addPiece b coordinates piece

iterateDirection' :: Board -> Color -> Coordinates -> Coordinates -> [Coordinates] -> [Coordinates]
iterateDirection' board color square@(x, y) direction@(dx ,dy) squares
    | not $ isInsideBoard square = squares
    | isEmpty board square = iterateDirection' board color (x + dx, y + dy) direction (square : squares)
    | otherwise = if isColor board square color then
                      squares
                  else
                      square : squares

iterateDirection :: Board -> Color -> Coordinates -> Coordinates -> [Coordinates]
iterateDirection board color (x, y) direction@(dx, dy) = iterateDirection' board color (x + dx, y + dy) direction []

getReachable :: Board -> Piece -> Coordinates -> [Coordinates]
getReachable board piece@(Piece Pawn _) square = moveSquares square piece
getReachable board piece@(Piece Knight _) square = moveSquares square piece
getReachable board piece@(Piece King _) square = moveSquares square piece
getReachable board piece@(Piece _ color) square = nub $ sort $ concatMap (iterateDirection board color square) (moveDirections piece)

canMove :: Board -> Piece -> Coordinates -> Coordinates -> Bool
canMove board piece start end = isInsideBoard start && isInsideBoard end
                                && isEmpty board end && end `elem` getReachable board piece start

canCapture :: Board -> Piece -> Coordinates -> Coordinates -> Bool
canCapture board (Piece _ color) start end = isColor board end (opponent color) && threatens board end start

threatens :: Board -> Coordinates -> Coordinates -> Bool
threatens board end start = case piece of
                              Piece Pawn color -> isInsideBoard start && isInsideBoard end && end `elem` attackSquares start piece
                              _ -> isInsideBoard start && isInsideBoard end && end `elem` getReachable board piece start
    where (Just piece) = getPiece board start

isChecked :: Board -> Color -> Coordinates -> Bool
isChecked board player square = any (threatens board square) opponentSquares
    where allSquares = indices board
          opponentSquares = filter filterOpponent allSquares
          filterOpponent x = case getPiece board x of
                               Just (Piece _ color) -> color == opponent player
                               Nothing -> False

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

parseBoard' :: String -> [Maybe Piece]
parseBoard' [] = []
parseBoard' ('\n':xs) = parseBoard' xs
parseBoard' (x:xs) = parsePiece x : parseBoard' xs

parseBoard :: String -> Maybe Board
parseBoard str | length pieces == 64 = Just $ boardFromPieces pieces
               | otherwise = Nothing
    where pieces = parseBoard' str

debugPrint :: Board -> IO ()
debugPrint = putStrLn . printBigPrettyBoard

boardFromPieces :: [Maybe Piece] -> Board
boardFromPieces pieces = listArray ((0, 0), (7, 7)) $ map f pieces
    where f (Just piece) = Square piece
          f Nothing = Empty

coordinatesToString :: Coordinates -> String
coordinatesToString (r, c) = [chr (ord 'a' + c), intToDigit (8 - r)]
