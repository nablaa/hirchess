module Move (GameState(..), Move(..), MoveType(..), Castling(..),
             getMove, isLegalMove, getAllLegalMoves, applyMoveBoard,
             longAlgebraicNotation, parseLongAlgebraicNotation, debugPrintMoves) where

import Data.Maybe
import Data.List
import Board
import Piece

data Move = Move MoveType Piece Coordinates Coordinates
            deriving (Eq, Show, Read)

data MoveType = Movement | Capture | Castling Castling | EnPassant Coordinates | Promotion Piece | PawnDoubleMove
                deriving (Eq, Show, Read)

data Castling = Long Color | Short Color
                deriving (Eq, Show, Read)

data GameState = State {
      board :: Board
    , player :: Color
    , castlingsPossible :: [Castling]
    , enPassantSquare :: Maybe Coordinates
    , halfmoveClock :: Integer
    , moveNumber :: Integer
    } deriving (Eq, Show, Read)

-- TODO Fix: queen
changeToPromotionMove :: Move -> Move
changeToPromotionMove (Move _ piece@(Piece Pawn color) start end) | isPromotionSquare end color = Move (Promotion (Piece Queen color)) piece start end
changeToPromotionMove move = move

changeToPawnDoubleMove :: Move -> Move
changeToPawnDoubleMove (Move Movement piece@(Piece Pawn color) start end) | isDoubleMove start end color = Move PawnDoubleMove piece start end
changeToPawnDoubleMove move = move

getAllLegalMoves :: GameState -> [Move]
getAllLegalMoves state = filter (isLegalMove state) $ catMaybes [getMove state start end | start <- allCoordinates, end <- allCoordinates]

getMove :: GameState -> Coordinates -> Coordinates -> Maybe Move
getMove state@(State board player castlings enpassant _ _) start end | piece == Nothing = Nothing
                                                                     | color /= Just player = Nothing
                                                                     | null moves = Nothing
                                                                     | length moves > 1 = error $ "Too many possible moves: " ++ show moves
                                                                     | length moves == 1 = Just $ changeToPawnDoubleMove $ changeToPromotionMove $ head moves
    where piece = getPiece board start
          color = getPlayer board start
          moves = catMaybes[getMovementMove state start end,
                            getCaptureMove state start end,
                            getCastleMove state start end,
                            getEnPassantMove state start end]

isPossibleMove :: GameState -> Move -> Bool
isPossibleMove state move@(Move _ _ start end) = generatedMove /= Nothing && fromJust generatedMove == move
    where generatedMove = getMove state start end

isLegalMove :: GameState -> Move -> Bool
isLegalMove state@(State board player _ _ _ _) move = not (isCheck newBoard player) && isPossibleMove state move && moveGetPlayer move == player
    where newBoard = applyMoveBoard board move

moveGetPlayer :: Move -> Color
moveGetPlayer (Move _ (Piece _ color) _ _) = color

applyMoveBoard :: Board -> Move -> Board
applyMoveBoard board (Move Movement _ start end) = movePiece board start end
applyMoveBoard board (Move Capture _ start end) = movePiece board start end
applyMoveBoard board (Move (Castling (Long White)) _ start end) = movePiece (movePiece board (7, 4) (7, 2)) (7, 0) (7, 3)
applyMoveBoard board (Move (Castling (Short White)) _ start end) = movePiece (movePiece board (7, 4) (7, 6)) (7, 7) (7, 5)
applyMoveBoard board (Move (Castling (Long Black)) _ start end) = movePiece (movePiece board (0, 4) (0, 2)) (0, 0) (0, 3)
applyMoveBoard board (Move (Castling (Short Black)) _ start end) = movePiece (movePiece board (0, 4) (0, 6)) (0, 7) (0, 5)
applyMoveBoard board (Move (EnPassant _) piece start end) = removePiece (movePiece board start end) (toEnPassantTargetSquare end)
applyMoveBoard board (Move (Promotion promoted) piece start end) = addPiece (removePiece board start) end promoted
applyMoveBoard board (Move PawnDoubleMove piece start end) = movePiece board start end


getMovementMove :: GameState -> Coordinates -> Coordinates -> Maybe Move
getMovementMove (State board _ _ _ _ _) start end | canMove board piece start end = Just $ Move Movement piece start end
                                                  | otherwise = Nothing
                                                       where (Just piece) = getPiece board start

getCaptureMove :: GameState -> Coordinates -> Coordinates -> Maybe Move
getCaptureMove (State board _ _ _ _ _) start end | canCapture board piece start end = Just $ Move Capture piece start end
                                                 | otherwise = Nothing
                                                 where (Just piece) = getPiece board start

getCastleMove :: GameState -> Coordinates -> Coordinates -> Maybe Move
getCastleMove (State _ _ [] _ _ _) _ _ = Nothing
getCastleMove (State board player castlings _ _ _) start end
    | startPiece /= Just (Piece King player) = Nothing
    | endPiece /= Just (Piece Rook player) = Nothing
    | castling' == Nothing = Nothing
    | castling `notElem` castlings = Nothing
    | any (isChecked board player) squares = Nothing
    | not $ all (isEmpty board) (squares \\ [start, end]) = Nothing
    where startPiece = getPiece board start
          endPiece = getPiece board end
          (Just king) = startPiece
          (Just rook) = endPiece
          castling' = getCastling player start end
          (Just castling) = castling'
          squares = getCastlingSquares castling
getCastleMove (State board player _ _ _ _) start end = Just $ Move (Castling castling) piece start end
    where (Just piece) = getPiece board start
          (Just castling) = getCastling player start end

getEnPassantMove :: GameState -> Coordinates -> Coordinates -> Maybe Move
getEnPassantMove (State _ _ _ Nothing _ _) _ _ = Nothing
getEnPassantMove (State board player _ (Just square) _ _) start end
    | startPiece /= Just (Piece Pawn player) = Nothing
    | endPiece /= Nothing = Nothing
    | end `notElem` captureSquares board piece start = Nothing
    | end /= square = Nothing
    | targetPiece /= Just (Piece Pawn (opponent player)) = Nothing
    where startPiece = getPiece board start
          (Just piece) = startPiece
          endPiece = getPiece board end
          targetSquare = toEnPassantTargetSquare end
          targetPiece = getPiece board targetSquare
getEnPassantMove (State board player _ _ _ _) start end = Just $ Move (EnPassant end) piece start end
    where (Just piece) = getPiece board start

getCastling :: Color -> Coordinates -> Coordinates -> Maybe Castling
getCastling White (7, 4) (7, 0) = Just (Long White)
getCastling White (7, 4) (7, 7) = Just (Short White)
getCastling Black (0, 4) (0, 0) = Just (Long Black)
getCastling Black (0, 4) (0, 7) = Just (Short Black)
getCastling _ _ _ = Nothing

getCastlingSquares :: Castling -> [Coordinates]
getCastlingSquares (Long White) = [(7, c) | c <- [0..4]]
getCastlingSquares (Short White) = [(7, c) | c <- [4..7]]
getCastlingSquares (Long Black) = [(0, c) | c <- [0..4]]
getCastlingSquares (Short Black) = [(0, c) | c <- [4..7]]

longAlgebraicNotation' :: Move -> String -> String
longAlgebraicNotation' (Move _ (Piece pieceType _) start end) separator = pieceStr ++ startStr ++ separator ++ endStr
    where pieceStr = case pieceType of
                       Pawn -> ""
                       _ -> [printPiece (Piece pieceType White)]
          startStr = coordinatesToString start
          endStr = coordinatesToString end

longAlgebraicNotation :: Move -> String
longAlgebraicNotation move@(Move Movement _ _ _) = longAlgebraicNotation' move "-"
longAlgebraicNotation move@(Move Capture _ _ _) = longAlgebraicNotation' move "x"
longAlgebraicNotation (Move (Castling (Long _)) _ _ _) = "O-O-O"
longAlgebraicNotation (Move (Castling (Short _)) _ _ _) = "O-O"
longAlgebraicNotation move@(Move (EnPassant _) _ _ _) = longAlgebraicNotation' move "x"
longAlgebraicNotation move@(Move (Promotion (Piece promoted _)) _ _ _) = longAlgebraicNotation' move "-" ++ [printPiece (Piece promoted White)]
longAlgebraicNotation move@(Move PawnDoubleMove _ _ _) = longAlgebraicNotation' move "-"

parseLongAlgebraicNotation :: String -> Maybe (Coordinates, Coordinates)
parseLongAlgebraicNotation (c1:r1:'-':c2:r2:[]) = do coord1 <- coord1'
                                                     coord2 <- coord2'
                                                     Just (coord1, coord2)
    where coord1' = stringToCoordinates [c1, r1]
          coord2' = stringToCoordinates [c2, r2]
parseLongAlgebraicNotation _ = Nothing

debugPrintMoves :: [Move] -> String
debugPrintMoves = unlines . map longAlgebraicNotation
