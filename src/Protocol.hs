module Protocol where

class Connection a where
        connect :: a -> IO a
        disconnect :: a -> IO a
        readMessage :: a -> IO String
        writeMessage :: a -> String -> IO ()
