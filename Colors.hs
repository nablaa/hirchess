module Colors (withColor, boardColor, coordinateColor,
               whitePlayerColor, blackPlayerColor, promptColor, errorColor) where

type ANSIColor = String

reset ="\x1b[0m"

boldOn ="\x1b[1m"
italicsOn ="\x1b[3m"
underlineOn ="\x1b[4m"
inverseOn ="\x1b[7m"
strikethroughOn ="\x1b[9m"
boldOff ="\x1b[22m"
italicsOff ="\x1b[23m"
underlineOff ="\x1b[24m"
inverseOff ="\x1b[27m"
strikethroughOff ="\x1b[29m"

black ="\x1b[30m"
red ="\x1b[31m"
green ="\x1b[32m"
yellow ="\x1b[33m"
blue ="\x1b[34m"
magenta ="\x1b[35m"
cyan ="\x1b[36m"
white ="\x1b[37m"
defaultColor ="\x1b[39m"
blackBG ="\x1b[40m"
redBG ="\x1b[41m"
greenBG ="\x1b[42m"
yellowBG ="\x1b[43m"
blueBG ="\x1b[44m"
magentaBG ="\x1b[45m"
cyanBG ="\x1b[46m"
whiteBG ="\x1b[47m"
defaultBG ="\x1b[49m"


boardColor = yellow
coordinateColor = cyan
whitePlayerColor = red
blackPlayerColor = blue
promptColor = magenta
errorColor = red


withColor :: ANSIColor -> String -> String
withColor color str = color ++ str ++ reset
