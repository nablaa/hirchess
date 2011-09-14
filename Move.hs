module Move (GameState(..), Move(..), MoveType(..),
             getMove, getAllLegalMoves, longAlgebraicNotation, debugPrintMoves) where

import Data.Maybe
import Data.List
import Board
import Piece

data Move = Move MoveType Piece Coordinates Coordinates
            deriving (Eq, Show, Read)

data MoveType = Movement | Capture | Castling Castling | EnPassant Coordinates | Promotion Piece | PawnDoubleMove
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
isLegalMove state@(State board player _ _ _ _) move = not (isCheck newBoard player) && isPossibleMove state move
    where newBoard = applyMoveBoard board move

applyMoveBoard :: Board -> Move -> Board
applyMoveBoard board (Move Movement _ start end) = movePiece board start end
applyMoveBoard board (Move Capture _ start end) = movePiece board start end
applyMoveBoard board (Move (Castling (Long White)) _ start end) = movePiece (movePiece board (7, 4) (7, 2)) (7, 0) (7, 3)
applyMoveBoard board (Move (Castling (Short White)) _ start end) = movePiece (movePiece board (7, 4) (7, 6)) (7, 7) (7, 5)
applyMoveBoard board (Move (Castling (Long Black)) _ start end) = movePiece (movePiece board (0, 4) (0, 2)) (0, 0) (0, 3)
applyMoveBoard board (Move (Castling (Short Black)) _ start end) = movePiece (movePiece board (0, 4) (0, 6)) (0, 7) (0, 5)
applyMoveBoard board (Move (EnPassant targetSquare) piece start end) = removePiece (movePiece board start end) targetSquare
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
    | end `notElem` attackSquares start piece = Nothing
    | targetSquare /= square = Nothing
    | targetPiece /= Just (Piece Pawn (opponent player)) = Nothing
    where startPiece = getPiece board start
          (Just piece) = startPiece
          endPiece = getPiece board end
          targetSquare = getEnPassantTargetSquare end player
          targetPiece = getPiece board targetSquare
getEnPassantMove (State board player _ _ _ _) start end = Just $ Move (EnPassant targetSquare) piece start end
    where (Just piece) = getPiece board start
          targetSquare = getEnPassantTargetSquare end player


longAlgebraicNotation' :: Move -> String -> String
longAlgebraicNotation' (Move _ (Piece pieceType _) start end) separator = pieceStr ++ startStr ++ separator ++ endStr
    where pieceStr = pieceTypeString pieceType
          startStr = coordinatesToString start
          endStr = coordinatesToString end

longAlgebraicNotation :: Move -> String
longAlgebraicNotation move@(Move Movement _ _ _) = longAlgebraicNotation' move "-"
longAlgebraicNotation move@(Move Capture _ _ _) = longAlgebraicNotation' move "x"
longAlgebraicNotation (Move (Castling (Long _)) _ _ _) = "O-O-O"
longAlgebraicNotation (Move (Castling (Short _)) _ _ _) = "O-O"
longAlgebraicNotation move@(Move (EnPassant _) _ _ _) = longAlgebraicNotation' move "x"
longAlgebraicNotation move@(Move (Promotion (Piece promoted _)) _ _ _) = longAlgebraicNotation' move "-" ++ pieceTypeString promoted
longAlgebraicNotation move@(Move PawnDoubleMove _ _ _) = longAlgebraicNotation' move "-"

debugPrintMoves :: [Move] -> IO ()
debugPrintMoves = mapM_ (putStrLn . longAlgebraicNotation)