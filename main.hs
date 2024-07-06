import System.IO
import Data.Char
import Data.List.Split

main = do
    (jsonFileName, nixFileName) <- getFileNames
    handle <- openFile jsonFileName ReadMode
    contents <- hGetContents handle
    let nixContent = json2nix contents
    writeFile nixFileName nixContent
    hClose handle

getFileNames = do
    putStrLn "--- step 1 of 2 ---"
    putStr "File name: "
    jsonFileName <- getLine
    putStrLn ""
    let nixFileNameSuggestion = jsonFileNameToNixFileName jsonFileName
    putStrLn "--- step 2 of 2 ---"
    putStrLn "Enter a name for the generated nix file"
    putStr ("Nix file name (" ++ nixFileNameSuggestion ++ "): ")
    nixFileName <- getLine
    let nixFileNameResult = if nixFileName == ""
        then nixFileNameSuggestion 
        else nixFileName
    return (jsonFileName, nixFileNameResult)

jsonFileNameToNixFileName :: String -> String
jsonFileNameToNixFileName s = let
  parts = splitOn "." s
  in head parts ++ ".nix"

json2nix :: String -> String
json2nix input = let
  jsonLines = lines input
  parsedLines = jsonLinesToLines jsonLines initContext
  in unlines $ map transformLine parsedLines

data Context where
  Context :: {listLevel :: Int} -> Context
  deriving Show
  
data Line = 
  ListStart String | ListEnd String | AttrSetStart String | AttrSetEnd String | AttrValue String | ListValue String
  deriving Show

commaToSemicolen :: [Char] -> [Char]
commaToSemicolen s = if last s == ',' then init s ++ ";" else s

unwrapValue :: String -> String
unwrapValue line = let
  parts = splitOn ":" line
  wrappedValue = head parts
  value = filter (/= '"') wrappedValue
  unwrappedLine = value ++ " =" ++ unwords (tail parts)
  in if length parts > 1 
    then unwrappedLine 
    else line

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
    _ -> if inList 
      then ListValue s 
      else AttrValue s

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
transformLine (AttrSetStart s) = unwrapValue s
transformLine (AttrSetEnd s) = endOnSemicolen s
transformLine (ListStart s) = unwrapValue s
transformLine (ListEnd s) = unwrapValue $ endOnSemicolen s
transformLine (ListValue s) = unwrapValue $ endOnNothing s
transformLine (AttrValue s) = unwrapValue $ endOnSemicolen s


