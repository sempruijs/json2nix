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

-- json2nix input = unlines $ map transformLine $ lines input
json2nix :: String -> String
json2nix input = let
  jsonLines = lines input
  parsedLines = jsonLinesToLines jsonLines initContext
  in unlines $ map transformLine parsedLines

data Context where
  Context :: {listLevel :: Int} -> Context

data Line = ListStart String | ListEnd String | AttrSetStart String | AttrSetEnd String | AttrValue String | ListValue String

commaToSemicolen :: [Char] -> [Char]
commaToSemicolen s = if last s == ',' then init s ++ ";" else s

unwrapValue line = let
  parts = splitOn ":" line
  wrappedValue = head parts
  value = filter (/= '"') wrappedValue
  unwrappedLine = value ++ " =" ++ (unwords $ tail parts)
  in if length parts > 1 then unwrappedLine else line

-- transformLine line = unwrapValue $ commaToSemicolen line

removeSemicolen s = if last s == ';' then  init s else s

initContext = Context { listLevel = 0 }

updateContext :: Line -> Context -> Context
updateContext (ListEnd _) c = Context {listLevel = listLevel c - 1}
updateContext (ListStart _) c = Context {listLevel = listLevel c + 1}
updateContext _ c = c

jsonLineToLine :: String -> Context -> Line
jsonLineToLine s c = let
  inList = listLevel c > 0
  lastChar = last s
  in case lastChar of
    '[' -> ListStart s
    ']' -> ListEnd s
    '{' -> AttrSetStart s
    '}' -> AttrSetEnd s
    _ -> if inList then AttrValue s else ListValue s

jsonLinesToLines :: [String] -> Context -> [Line]
jsonLinesToLines [] _ = []
jsonLinesToLines [x] c = [jsonLineToLine x c]
jsonLinesToLines (x:xs) c = let
  line = jsonLineToLine x c
  newContext = updateContext line c
  in line : jsonLinesToLines xs newContext



endOnSemicolen :: String -> String
endOnSemicolen s = let
  lastChar = last s
  in case lastChar of
    ',' -> init s ++ [';']
    ';' -> s
    _ -> s ++ [';']

endOnNothing :: String -> String
endOnNothing s = let
  lastChar = last s
  in case lastChar of
    -- TODO: write as one
    ',' -> init s
    ';' -> init s
    _ -> s

transformLine :: Line -> String
transformLine (AttrSetStart s) = s
transformLine (AttrSetEnd s) = endOnSemicolen s
transformLine (ListStart s) = s
transformLine (ListEnd s) = endOnSemicolen s
transformLine (ListValue s) = endOnNothing s
transformLine (AttrValue s) = endOnSemicolen s


