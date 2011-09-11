import Test.QuickCheck
import Test.QuickCheck.Batch
import Board
import Piece

data Coordinate = Coordinate Int

instance Show Coordinate where
    show (Coordinate n) = show n

instance Arbitrary Coordinate where
    arbitrary = do
      n <- choose (0, 7)
      return (Coordinate n)

instance Arbitrary Color where
    arbitrary = do
      n <- choose (0, 1) :: Gen Integer
      case n of
        0 -> return White
        1 -> return Black

prop_pawnDoubleMove' column piece start end = canMove initialBoard piece (start, column) (end, column) == True

prop_pawnDoubleMove (Coordinate column) color = case color of
                                                  White -> prop_pawnDoubleMove' column (Piece Pawn White) 6 4
                                                  Black -> prop_pawnDoubleMove' column (Piece Pawn Black) 1 3




options = TestOptions
          { no_of_tests         = 200
          , length_of_tests     = 1
          , debug_tests         = False }

main = do
  runTests "complex" options
       [ run prop_pawnDoubleMove
        ]
