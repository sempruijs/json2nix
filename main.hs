import System.IO
import Data.Char
import Data.List.Utils (replace)

main = do
    putStr "File name: "
    fileName <- getLine
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    putStr $ json2nix contents
    hClose handle

json2nix :: String -> String
json2nix input = replace "e" "x" input

uppercase :: String -> String
uppercase = map toUpper


applySemicolen :: String -> String

colenToEqual :: String -> String

stripLeadingWhitespace :: String -> String
stripLeadingWhitespace = unlines . map (dropWhile isSpace) . lines

-- "hello" = true; -> hello = true;
stringToValue :: String -> String

isStartList :: String -> Bool

isEndListt :: String -> Bool

