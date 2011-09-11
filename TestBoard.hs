import Test.QuickCheck
import Test.QuickCheck.Batch
import Board
import Piece
import Data.List

data Coordinate = Coordinate Int

instance Show Coordinate where
    show (Coordinate n) = show n

instance Arbitrary Coordinate where
    arbitrary = do
      n <- choose (0, 7)
      return (Coordinate n)

instance Arbitrary Color where
    arbitrary = do
      n <- choose (0, 1) :: Gen Int
      case n of
        0 -> return White
        1 -> return Black

instance Arbitrary Type where
    arbitrary = do
      n <- choose (0, 5) :: Gen Int
      case n of
        0 -> return Pawn
        1 -> return Rook
        2 -> return Knight
        3 -> return Bishop
        4 -> return Queen
        5 -> return King

allSquares = do
  x <- [0..7]
  y <- [0..7]
  return (x, y)


prop_pawnDoubleMove' column piece start end = canMove initialBoard piece (start, column) (end, column) == True

prop_pawnDoubleMove (Coordinate column) color = case color of
                                                  White -> prop_pawnDoubleMove' column (Piece Pawn White) 6 4
                                                  Black -> prop_pawnDoubleMove' column (Piece Pawn Black) 1 3

equal a b = nub (sort a) == nub (sort b)

prop_crosscheckMoveSquares (Coordinate x, Coordinate y) pieceType color
    = equal (moveSquares coordinates piece) (getReachable emptyBoard piece coordinates)
      where coordinates = (x, y)
            piece = (Piece pieceType color)


{-
   +----+----+----+----+----+----+----+----+
8  |    |    |    |    |    |    |    |    |
   +----+----+----+----+----+----+----+----+
7  |    |    |    |    | NB |    |    |    |
   +----+----+----+----+----+----+----+----+
6  |    |    |    |    |    |    |    |    |
   +----+----+----+----+----+----+----+----+
5  |    |    |    |    |    |    |    |    |
   +----+----+----+----+----+----+----+----+
4  |    | QW |    |    |    |    |    |    |
   +----+----+----+----+----+----+----+----+
3  |    |    |    |    |    |    |    |    |
   +----+----+----+----+----+----+----+----+
2  |    | KW |    |    |    |    |    |    |
   +----+----+----+----+----+----+----+----+
1  |    |    |    |    |    |    |    |    |
   +----+----+----+----+----+----+----+----+
     a    b    c    d    e    f    g    h
-}
test_queenMove = equal squares moveable
    where board = addPieces emptyBoard [(start, piece),
                                        ((1, 4), (Piece Knight Black)),
                                        ((6, 1), (Piece King White))]
          piece = (Piece Queen White)
          start = (4, 1)
          moveable = [(0, 1), (1, 1), (2, 1), (2, 3), (3, 0), (3, 1), (3, 2), (4, 0), (4, 2), (4, 3), (4, 4), (4, 5), (4, 6), (4, 7), (5, 0), (5, 1), (5, 2), (6, 3), (7, 4)]
          squares = filter (canMove board piece start) allSquares


options = TestOptions
          { no_of_tests         = 200
          , length_of_tests     = 1
          , debug_tests         = False }

main = do
  runTests "complex" options
       [ run prop_pawnDoubleMove
       , run prop_crosscheckMoveSquares
       , run test_queenMove
        ]
