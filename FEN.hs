module FEN (writeFEN, readFEN) where

import Data.Char
import Data.Maybe
import Data.List
import Move
import Board
import Piece

writeBoard :: Board -> String
writeBoard = intercalate "/" . lines . concatMap emptyToNum . group . printBoardCompact
    where emptyToNum str@(' ':_) = show $ length str
          emptyToNum str = str

writePlayer :: Color -> String
writePlayer White = "w"
writePlayer Black = "b"

writeCastlings :: [Castling] -> String
writeCastlings = sort . concatMap toString
    where toString (Long White) = "Q"
          toString (Short White) = "K"
          toString (Long Black) = "q"
          toString (Short Black) = "k"

writeEnPassant :: Maybe Coordinates -> String
writeEnPassant Nothing = "-"
writeEnPassant (Just square) = coordinatesToString square

writeFEN :: GameState -> String
writeFEN state = unwords [writeBoard (board state), writePlayer (player state), writeCastlings (castlingsPossible state), writeEnPassant (enPassantSquare state), show (halfmoveClock state), show (moveNumber state)]

readFEN :: String -> Maybe GameState
readFEN str | length parts /= 6 = Nothing
            | otherwise = do board <- readBoard $ head parts
                             player <- readPlayer $ parts !! 1
                             castlings <- readCastlings $ parts !! 2
                             enPassant <- readEnPassant $ parts !! 3
                             halfmoves <- maybeRead $ parts !! 4
                             moves <- maybeRead $ parts !! 5
                             return $ State board player castlings enPassant halfmoves moves []
    where parts = words str

readBoard :: String -> Maybe Board
readBoard str | length parts /= 8 = Nothing
              | otherwise = parseBoardCompact $ unlines parts
    where numToEmpty x | isNumber x = replicate (digitToInt x) ' '
                       | otherwise = [x]
          parts = split (== '/') $ concatMap numToEmpty str

readPlayer :: String -> Maybe Color
readPlayer "w" = Just White
readPlayer "b" = Just Black
readPlayer _ = Nothing

readCastlings :: String -> Maybe [Castling]
readCastlings = mapM toCastling
    where toCastling 'Q' = Just $ Long White
          toCastling 'K' = Just $ Short White
          toCastling 'q' = Just $ Long Black
          toCastling 'k' = Just $ Short Black
          toCastling _ = Nothing

readEnPassant :: String -> Maybe (Maybe Coordinates)
readEnPassant "-" = Just Nothing
readEnPassant str = case stringToCoordinates str of
                      Nothing -> Nothing
                      coordinates -> Just coordinates

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads

split :: (Char -> Bool) -> String -> [String]
split p str = case dropWhile p str of
                "" -> []
                str' -> w : split p str''
                    where (w, str'') = break p str'
