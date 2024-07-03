import System.IO
import Data.Char
import Data.List.Split

main = do
    putStr "File name: "
    fileName <- getLine
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    putStr $ json2nix contents
    hClose handle

json2nix input = unlines $ map transformLine $ lines input

uppercase :: String -> String
uppercase = map toUpper

isStartList s = last s == '['

isEndList s = last s == ']'

commaToSemicolen :: [Char] -> [Char]
commaToSemicolen s = if (last s == ',') then (init s ++ ";") else s

unwrapValue line = let
  parts = splitOn ":" line
  wrappedValue = head parts
  value = filter (/= '"') wrappedValue
  unwrappedLine = value ++ " =" ++ (unwords $ tail parts)
  in if length parts > 1 then unwrappedLine else line

transformLine line = unwrapValue $ commaToSemicolen line


