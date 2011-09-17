module Board (Board, Coordinates, initialBoard, emptyBoard,
              printBoard, parseBoard, printPrettyBoard, printBigPrettyBoard, printBigPrettyColoredBoard,
              printSquares, canMove, canCapture, getPiece, getPlayer, isColor, getReachable, addPiece,
              removePiece, movePiece, addPieces, isChecked, isCheck, isEmpty, debugPrintBoard,
              coordinatesToString, stringToCoordinates, allCoordinates) where

import Data.Char
import Data.Maybe
import Data.Array
import Data.List
import Piece
import Colors

data Square = Square Piece | Empty
              deriving (Eq, Show, Read)

type Coordinates = (Int, Int)
type Board = Array Coordinates Square


boardColor = yellow
coordinateColor = cyan

initialBoard :: Board
initialBoard = listArray ((0, 0), (7, 7)) rows
    where officerRow p = map (\x -> Square (Piece x p)) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
          pawnRow p = map (\x -> Square (Piece x p)) $ replicate 8 Pawn
          rows = officerRow Black ++ pawnRow Black ++ replicate 32 Empty ++ pawnRow White ++ officerRow White

emptyBoard :: Board
emptyBoard = listArray ((0, 0), (7, 7)) (repeat Empty)

allCoordinates :: [Coordinates]
allCoordinates = indices emptyBoard

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
addPiece board coordinates = updateBoard board coordinates . Square

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

isCheck :: Board -> Color -> Bool
isCheck board player = isChecked board player kingSquare
    where kingSquare = fromJust $ rlookup (Square (Piece King player)) $ assocs board
          rlookup x = lookup x . map swap
          swap (x, y) = (y, x)

squareToChar :: Square -> Char
squareToChar Empty = ' '
squareToChar (Square p) = printPiece p

printBoard :: Board -> String
printBoard board = toLines $ foldr f "" (elems board)
    where f = (:) . squareToChar
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

printBigPrettyColoredBoard :: Board -> String
printBigPrettyColoredBoard = addCoordinatesColored . printRowsColored . intoRows . elems
    where intoRows [] = []
          intoRows xs = take 8 xs : intoRows (drop 8 xs)

addCoordinatesColored :: String -> String
addCoordinatesColored str = unlines (init (zipWith (++) numbers (lines str))) ++ cColor "     a    b    c    d    e    f    g    h\n"
    where cColor = withColor coordinateColor
          numbers = (map cColor . concat . transpose) [repeat "   ", reverse [intToDigit n : "  " | n <- [1..8]]]

printSquareColored :: Square -> String
printSquareColored Empty = "    "
printSquareColored (Square p) = " " ++ printBigPieceColored p ++ " "

printRowColored :: [Square] -> String
printRowColored row = sep ++ intercalate sep (map printSquareColored row) ++ sep ++ "\n"
    where bColor = withColor boardColor
          sep = bColor "|"

printRowsColored :: [[Square]] -> String
printRowsColored rows = line ++ intercalate line (map printRowColored rows) ++ line
    where bColor = withColor boardColor
          line = bColor $ concat (replicate 8 "+----") ++ "+\n"

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

debugPrintBoard :: Board -> IO ()
debugPrintBoard = putStrLn . printBigPrettyBoard

boardFromPieces :: [Maybe Piece] -> Board
boardFromPieces pieces = listArray ((0, 0), (7, 7)) $ map f pieces
    where f (Just piece) = Square piece
          f Nothing = Empty

coordinatesToString :: Coordinates -> String
coordinatesToString (r, c) = [chr (ord 'a' + c), intToDigit (8 - r)]

stringToCoordinates :: String -> Maybe Coordinates
stringToCoordinates (c:r:[]) | isInsideBoard coordinates = Just coordinates
                             | otherwise = Nothing
    where coordinates = (ord '8' - ord r, ord c - ord 'a')
stringToCoordinates _ = Nothing
