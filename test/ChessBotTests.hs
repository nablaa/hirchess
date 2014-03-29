module ChessBotTests where

import Test.Hspec
import ChessBot

chessBotSpec :: IO ()
chessBotSpec = hspec $
        describe "ChessBot"
          parseCommandSpec

parseCommandSpec :: Spec
parseCommandSpec =
        describe "parseCommand" $ do
          it "should not parse invalid commands" $ do
            parseCommand "foobar" `shouldBe` Nothing
            parseCommand "" `shouldBe` Nothing
            parseCommand "  " `shouldBe` Nothing
            parseCommand "!foo" `shouldBe` Nothing

          it "should not parse commands which don't start with !-character" $ do
            parseCommand "newgame" `shouldBe` Nothing
            parseCommand "move" `shouldBe` Nothing
            parseCommand "help" `shouldBe` Nothing
            parseCommand "fen" `shouldBe` Nothing

          it "should parse !newgame command" $ do
            parseCommand "!newgame" `shouldBe` Just NewGame
            parseCommand "!newgame foo" `shouldBe` Nothing

          it "should parse !help command" $ do
            parseCommand "!help" `shouldBe` Just Help
            parseCommand "!help foo" `shouldBe` Nothing

          it "should parse !board command" $ do
            parseCommand "!board" `shouldBe` Just Board
            parseCommand "!board foo" `shouldBe` Nothing

          it "should parse !status command" $ do
            parseCommand "!status" `shouldBe` Just Status
            parseCommand "!status foo" `shouldBe` Nothing

          it "should parse !fen command" $ do
            parseCommand "!fen" `shouldBe` Just FEN
            parseCommand "!fen foo" `shouldBe` Nothing

          it "should parse !move command" $ do
            parseCommand "!move" `shouldBe` Nothing
            parseCommand "!move e2-e4" `shouldBe` Just (Move "e2-e4")
            parseCommand "!move e2-e4 foo" `shouldBe` Nothing
            parseCommand "!move Nf3" `shouldBe` Just (Move "Nf3")
            parseCommand "!move    O-O-O" `shouldBe` Just (Move "O-O-O")
