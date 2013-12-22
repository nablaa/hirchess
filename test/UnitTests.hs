import Test.HUnit
import Notation
import Move
import Piece

import System.Exit (exitFailure, exitSuccess)

testPrintingAlgebraicNotation :: Test
testPrintingAlgebraicNotation = TestList [
          "O-O-O" ~=? (printAlgebraicNotation (Move (Castling (Long White)) (Piece King White) (7, 4) (7, 0)))
        , "O-O" ~=? (printAlgebraicNotation (Move (Castling (Short Black)) (Piece King Black) (0, 4) (0, 7)))
        , "Nf3" ~=? (printAlgebraicNotation (Move Movement (Piece Knight White) (7, 6) (5, 5)))
        ]

tests :: Test
tests = testPrintingAlgebraicNotation

main :: IO ()
main = do c <- runTestTT $ tests
          if failures c > 0 || errors c > 0 then exitFailure
                                            else exitSuccess
