module ChessBotTests where

import Test.Hspec
import ChessBot
import Data.Maybe
import Chess
import Chess.FEN

chessBotSpec :: IO ()
chessBotSpec = hspec $
        describe "ChessBot" $ do
          parseCommandSpec
          commandOutputSpec

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
            parseCommand "  !move    O-O-O  " `shouldBe` Just (Move "O-O-O")

commandOutputSpec :: Spec
commandOutputSpec =
        describe "commandOutput" $ do
          it "should return initial bot state and message when new game command is given" $
            commandOutput testState NewGame `shouldBe` (initialBotState, "New game started")

          it "should not modify bot state when no state modifying command command is given" $ do
            fst (commandOutput testState Help) `shouldBe` testState
            fst (commandOutput testState Board) `shouldBe` testState
            fst (commandOutput testState Status) `shouldBe` testState
            fst (commandOutput testState FEN) `shouldBe` testState

          it "should return game FEN when FEN command is given" $
            snd (commandOutput testState FEN) `shouldBe` "FEN: " ++ writeFEN (botGameState testState)

          it "should not modify game state if invalid move is given" $
            botGameState (fst (commandOutput testState (Move "a2-a9"))) `shouldBe` botGameState testState

          it "should modify game state if valid move is given" $
            botGameState (fst (commandOutput testState (Move "e7-e5"))) `shouldBe` fromJust (move (botGameState testState) "e7-e5")


testState :: BotState
testState = BotState (fromJust (move newGame "e2-e4"))
