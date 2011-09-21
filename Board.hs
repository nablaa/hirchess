module Board (Board, Coordinates, initialBoard, parseBoardCompact, printBoardCompact,
              printBoard, printBoardColored, canMove, canCapture, getPiece, getPlayer,
              addPiece, removePiece, movePiece, isChecked, isCheck, isEmpty,
              coordinatesToString, stringToCoordinates, allCoordinates,
              isPromotionSquare, captureSquares, isDoubleMove,
              fromEnPassantTargetSquare, toEnPassantTargetSquare) where

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

initialBoard :: Board
initialBoard = listArray ((0, 0), (7, 7)) rows
    where officerRow p = map (\t -> Square (Piece t p)) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
          pawnRow p = map (\t -> Square (Piece t p)) $ replicate 8 Pawn
          rows = officerRow Black ++ pawnRow Black ++ replicate 32 Empty ++ pawnRow White ++ officerRow White

emptyBoard :: Board
emptyBoard = listArray ((0, 0), (7, 7)) (repeat Empty)

allCoordinates :: [Coordinates]
allCoordinates = indices emptyBoard

isInsideBoard :: Coordinates -> Bool
isInsideBoard (i, j) = i >= 0 && i <= 7 && j >= 0 && j <= 7

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

isPlayer :: Board -> Coordinates -> Color -> Bool
isPlayer b c color = Just color == getPlayer b c

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

sumCoordinates :: Coordinates -> Coordinates -> Coordinates
sumCoordinates (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

iterateDirection' :: Board -> Color -> Coordinates -> Coordinates -> [Coordinates] -> [Coordinates]
iterateDirection' board color square direction squares
    | not $ isInsideBoard square = squares
    | isEmpty board square = iterateDirection' board color (sumCoordinates square direction) direction (square : squares)
    | otherwise = if isPlayer board square color then
                      squares
                  else
                      square : squares

iterateDirection :: Board -> Color -> Coordinates -> Coordinates -> [Coordinates]
iterateDirection board color square direction = iterateDirection' board color (sumCoordinates square direction) direction []

moveSquares :: Board -> Piece -> Coordinates -> [Coordinates]
moveSquares _ piece@(Piece Pawn White) (6, c) = [(5, c), (4, c)]
moveSquares _ piece@(Piece Pawn Black) (1, c) = [(2, c), (3, c)]
moveSquares _ piece@(Piece Pawn _) square = filter isInsideBoard $ map (sumCoordinates square) (movePattern piece)
moveSquares _ piece@(Piece Knight _) square = filter isInsideBoard $ map (sumCoordinates square) (movePattern piece)
moveSquares _ piece@(Piece King _) square = filter isInsideBoard $ map (sumCoordinates square) (movePattern piece)
moveSquares board piece@(Piece _ color) square = nub $ sort $ concatMap (iterateDirection board color square) (movePattern piece)

captureSquares :: Board -> Piece -> Coordinates -> [Coordinates]
captureSquares _ piece@(Piece Pawn _) square = filter isInsideBoard $ map (sumCoordinates square) (capturePattern piece)
captureSquares _ piece@(Piece Knight _) square = filter isInsideBoard $ map (sumCoordinates square) (capturePattern piece)
captureSquares _ piece@(Piece King _) square = filter isInsideBoard $ map (sumCoordinates square) (capturePattern piece)
captureSquares board piece@(Piece _ color) square = nub $ sort $ concatMap (iterateDirection board color square) (movePattern piece)

canMove :: Board -> Piece -> Coordinates -> Coordinates -> Bool
canMove board piece start end = isInsideBoard start && isInsideBoard end
                                && isEmpty board end && end `elem` moveSquares board piece start

canCapture :: Board -> Piece -> Coordinates -> Coordinates -> Bool
canCapture board piece@(Piece _ color) start end = isInsideBoard start && isInsideBoard end
                                                   && isPlayer board end (opponent color) && end `elem` captureSquares board piece start

threatens :: Board -> Coordinates -> Coordinates -> Bool
threatens board end start = canCapture board piece start end
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

enPassantConversion :: Int -> Coordinates -> Coordinates
enPassantConversion n (r, c) | r <= 3 = (r + n, c)
                             | otherwise = (r - n, c)

fromEnPassantTargetSquare :: Coordinates -> Coordinates
fromEnPassantTargetSquare = enPassantConversion (-1)

toEnPassantTargetSquare :: Coordinates -> Coordinates
toEnPassantTargetSquare = enPassantConversion 1

isPromotionSquare :: Coordinates -> Color -> Bool
isPromotionSquare (0, _) White = True
isPromotionSquare (7, _) Black = True
isPromotionSquare _ _ = False

isDoubleMove :: Coordinates -> Coordinates -> Color -> Bool
isDoubleMove (6, _) (4, _) White = True
isDoubleMove (1, _) (3, _) Black = True
isDoubleMove _ _ _ = False

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

squareToChar :: Square -> Char
squareToChar Empty = ' '
squareToChar (Square p) = printPiece p

printBoardCompact :: Board -> String
printBoardCompact board = toLines $ foldr f "" (elems board)
    where f = (:) . squareToChar
          toLines [] = []
          toLines str = take 8 str ++ "\n" ++ toLines (drop 8 str)

printBoard :: Board -> String
printBoard = printBoard' False

printBoardColored :: Board -> String
printBoardColored = printBoard' True

printBoard' :: Bool -> Board -> String
printBoard' colored = addCoordinates colored . printRows colored . intoRows . elems
    where intoRows [] = []
          intoRows xs = take 8 xs : intoRows (drop 8 xs)

addCoordinates :: Bool -> String -> String
addCoordinates colored str = unlines (zipWith (++) numbers (lines str)) ++ cColor chars
    where numbers = map cColor $ lines $ unlines $ ["  \n" ++ intToDigit n : " " | n <- reverse [1..8]] ++ ["  "]
          chars = "    a   b   c   d   e   f   g   h\n"
          cColor | colored = withColor coordinateColor
                 | otherwise = id

printSquare :: Bool ->  Square -> String
printSquare _ Empty = "   "
printSquare True (Square p) = " " ++ printPieceColored p ++ " "
printSquare False (Square p) = " " ++ [printPiece p] ++ " "

printRow :: Bool -> [Square] -> String
printRow colored row = sep ++ intercalate sep (map (printSquare colored) row) ++ sep ++ "\n"
    where bColor | colored = withColor boardColor
                 | otherwise = id
          sep = bColor "|"

printRows :: Bool -> [[Square]] -> String
printRows colored rows = line ++ intercalate line (map (printRow colored) rows) ++ line
    where bColor | colored = withColor boardColor
                 | otherwise = id
          line = bColor $ concat (replicate 8 "+---") ++ "+\n"

printSquares :: (Board -> String) -> [Coordinates] -> String
printSquares f squares = f $ emptyBoard // [(s, Square (Piece Pawn White)) | s <- squares]

parseBoard' :: String -> [Maybe Piece]
parseBoard' [] = []
parseBoard' ('\n':xs) = parseBoard' xs
parseBoard' (x:xs) = parsePiece x : parseBoard' xs

parseBoardCompact :: String -> Maybe Board
parseBoardCompact str | length pieces == 64 = Just $ boardFromPieces pieces
                      | otherwise = Nothing
    where pieces = parseBoard' str
