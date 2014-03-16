module UI (printColoredState, printBoardUnicode, printBoardCompact, printBoard) where

import Chess
import Chess.FEN
import Colors
import Data.Array
import Data.Char
import Data.List

printColoredState :: GameState -> String
printColoredState state = printBoardColored (board state) ++ "\n\n"
                          ++ "Player: " ++ withColor (playerColor color) (show color)
                          ++ "              Move: " ++ show (fullMoveNumber state) ++ "\n"
                          ++ "FEN: " ++ writeFEN state
                          ++ winnerStatus
    where color = currentPlayer state
          playerColor White = whitePlayerColor
          playerColor Black = blackPlayerColor
          winnerStatus = case winner state of
                           Just player -> "\n" ++ withColor notificationColor "Game over. The winner is " ++ withColor (playerColor player) (show player)
                           Nothing -> ""

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
printSquare False (Square p) = " " ++ printPiece p ++ " "

printRow :: Bool -> [Square] -> String
printRow colored row = sep ++ intercalate sep (map (printSquare colored) row) ++ sep ++ "\n"
    where bColor | colored = withColor boardColor
                 | otherwise = id
          sep = bColor "|"

printRows :: Bool -> [[Square]] -> String
printRows colored rows = line ++ intercalate line (map (printRow colored) rows) ++ line
    where bColor | colored = withColor boardColor
                 | otherwise = id
          line = bColor (concat (replicate 8 "+---") ++ "+") ++ "\n"

printPiece :: Piece -> String
printPiece (Piece color t) = case color of
                               White -> [toUpper c]
                               Black -> [toLower c]
    where (Just c) = lookup t pieceChars

pieceChars :: [(PieceType, Char)]
pieceChars = [(Pawn, 'P'), (Knight, 'N'), (Bishop, 'B'), (Rook, 'R'), (Queen, 'Q'), (King, 'K')]

printPieceUTF8 :: Piece -> String
printPieceUTF8 (Piece color t) = case color of
                                  White -> [white]
                                  Black -> [black]
    where (Just white) = lookup t [(Pawn, '♙'), (Knight, '♘'), (Bishop, '♗'), (Rook, '♖'), (Queen, '♕'), (King, '♔')]
          (Just black) = lookup t [(Pawn, '♟'), (Knight, '♞'), (Bishop, '♝'), (Rook, '♜'), (Queen, '♛'), (King, '♚')]

printPieceColored :: Piece -> String
printPieceColored p@(Piece color _) = withColor (printColor color) (printPiece p)
    where printColor White = whitePlayerColor
          printColor Black = blackPlayerColor

printBoardUnicode :: Board -> String
printBoardUnicode b = toLines $ foldr f "" (elems b)
    where f = (:) . squareToUTF8Char
          toLines [] = []
          toLines str = take 8 str ++ "\n" ++ toLines (drop 8 str)

squareToUTF8Char :: Square -> Char
squareToUTF8Char Empty = ' '
squareToUTF8Char (Square p) = head $ printPieceUTF8 p

printBoardCompact :: Board -> String
printBoardCompact b = toLines $ foldr f "" (elems b)
    where f = (:) . squareToChar
          toLines [] = []
          toLines str = take 8 str ++ "\n" ++ toLines (drop 8 str)

squareToChar :: Square -> Char
squareToChar Empty = ' '
squareToChar (Square p) = head $ printPiece p
