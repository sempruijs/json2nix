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

instance Show Value where
  show (NullValue) = "null"
  show (IntValue a) = show a
  show (FloatValue a) = show a
  show (StringValue a) = show a
  show (BoolValue a) = show a
  show (ArrayValue xs) = "[\n" ++ unlines (map showAsNix xs) ++ "]"
  -- show (ObjectValue xs)

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
  NullValue -> "null"
  ArrayValue xs -> "[\n" ++ unlines (map showAsNix xs) ++ "]"
  _ -> "unsupported type"

type Index = Int

parseJson :: JsonInput -> Value
parseJson jsonInput =
  let
    nextValue :: JsonInput -> Index -> (Value, Index)
    nextValue input index = let
      indexChar = input !! index
      in if indexChar == ' ' || indexChar == ','
         then nextValue jsonInput (index + 1)
         else case indexChar of
                '"' -> let
                  (value, newIndex) = parseString input index
                  in (StringValue value, newIndex)
                'n' -> (NullValue, index + 4)
                '[' -> let
                  parseList :: JsonInput -> Index -> [Value] -> ([Value], Index)
                  parseList input1 i values = let
                    indexChar1 = input1 !! i
                    in case indexChar1 of
                      ' ' -> parseList input (i + 1) values
                      ',' -> parseList input (i + 1) values
                      ']' -> (values,index)
                      _ -> let
                        (value2, index2) = nextValue input1  (i + 1)
                        in parseList input1 index2  (values ++ [value2])
                    in let
                      (values3, index3) = parseList input (index + 1) []
                      in (ArrayValue values3, index3)
                c -> if isDigit c
                  then let
                  number = head $ words input
                  in (IntValue (read number :: Int), index)
                  else (StringValue "bla", index)
  in fst (nextValue jsonInput 0)



-- parseInt :: JsonInput -> Index -> (Int, Index)
-- parseFloat :: JsonInput -> Index -> (Float, Index)

parseString :: JsonInput -> Index -> (String, Index)
parseString input i = let
  value = (splitOn "\"" input) !! 1
  in (value, (length value) + i + 2)



