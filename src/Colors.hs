module Colors (ANSIColor, withColor, boardColor, coordinateColor,
               whitePlayerColor, blackPlayerColor, promptColor, errorColor,
               notificationColor) where

type ANSIColor = String

reset :: String
reset ="\x1b[0m"

red :: String
red ="\x1b[31m"

green :: String
green ="\x1b[32m"

yellow :: String
yellow ="\x1b[33m"

blue :: String
blue ="\x1b[34m"

magenta :: String
magenta ="\x1b[35m"

cyan :: String
cyan ="\x1b[36m"

white :: String
white ="\x1b[37m"

boardColor :: ANSIColor
boardColor = yellow

coordinateColor :: ANSIColor
coordinateColor = cyan

whitePlayerColor :: ANSIColor
whitePlayerColor = white

blackPlayerColor :: ANSIColor
blackPlayerColor = blue

promptColor :: ANSIColor
promptColor = magenta

errorColor :: ANSIColor
errorColor = red

notificationColor :: ANSIColor
notificationColor = green


withColor :: ANSIColor -> String -> String
withColor color str = color ++ str ++ reset
