module Main where

import Data.List.Split
import System.IO
import Data.Char

main :: IO ()
main = do
    (jsonFileName, nixFileName) <- getFileNames
    handle <- openFile jsonFileName ReadMode
    contents <- hGetContents handle
    let nixContent = json2nix contents
    writeFile nixFileName nixContent
    hClose handle

getFileNames :: IO (String, String)
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

data Value = NullValue | IntValue Int | FloatValue Float | BoolValue Bool | StringValue String | ArrayValue [Value] | ObjectValue [ObjectAttribute]

data ObjectAttribute = ObjectAttribute {
  name :: String,
  value :: Value
}

type JsonInput = String
type Nix = String

json2nix :: JsonInput -> Nix
json2nix s = let
  value = parseJson s
  in showAsNix value

showAsNix :: Value -> Nix
showAsNix v = case v of
  StringValue a -> a
  IntValue a -> show a
  _ -> "waf"

type Index = Int

parseJson :: JsonInput -> Value
parseJson jsonInput =
  let
    lastIndex = length jsonInput
    nextValue :: JsonInput -> Index -> (Value, Index)
    nextValue input index = let
      indexChar = input !! index
      in if indexChar == ' '
         then nextValue jsonInput (index + 1)
         else case indexChar of
                '\"' -> let
                  value = (StringValue $ (splitOn "\"" input) !! 1)
                  in (value, index)
                c -> if isDigit c
                  then let
                  number = words input !! 0
                  in (IntValue (read number :: Int), index)
                  else (StringValue "bla", index)
  in fst (nextValue jsonInput 0)

