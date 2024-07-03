import System.IO
import Data.Char

main = do
    putStr "File name: "
    fileName <- getLine
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    putStr $ json2nix contents
    hClose handle

json2nix :: String -> String
json2nix input = input

uppercase :: String -> String
uppercase = map toUpper

isStartList s = last s == '['
isEndList s = last s == ']'

commaToSemicolen s = if (last s == ',') then (init s ++ ";") else s


