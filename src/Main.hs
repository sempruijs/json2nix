module Main where

import Data.List.Split
import System.IO
import Data.Char
import Debug.Trace

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

data Value =
  NullValue
  | IntValue Int
  | FloatValue Float
  | BoolValue Bool
  | StringValue String
  | ArrayValue [Value]
  | ObjectValue [ObjectAttribute]

instance Show Value where
  show (NullValue) = "null"
  show (IntValue a) = show a
  show (FloatValue a) = show a
  show (StringValue a) = show a
  show (BoolValue a) = map toLower (show a)
  show (ArrayValue xs) = "[\n" ++ unlines (map (showAsNix 0) xs) ++ "]"
  show (ObjectValue attrs) = "{" ++ unlines (map show attrs) ++ "}"

data ObjectAttribute = ObjectAttribute String Value

instance Show ObjectAttribute where
  show (ObjectAttribute name value) = name ++ " = " ++ show value

showObjectAttr :: Int -> ObjectAttribute -> String
showObjectAttr i (ObjectAttribute name value) = (take (i * indentSpace) $ repeat ' ') ++ name ++ " = " ++ (showAsNix i value)

type JsonInput = String
type Nix = String

json2nix :: JsonInput -> Nix
json2nix s = let
  value = parseJson s
  in showAsNix 0 value

showAsNix :: Int -> Value -> Nix
showAsNix i v = case v of
  StringValue a -> a
  IntValue a -> show a
  NullValue -> "null"
  BoolValue a -> show a
  ArrayValue xs -> "[\n" ++ unlines (map (showAsNix i) xs) ++ "]"
  ObjectValue xs -> "{\n" ++ unlines (map (showObjectAttr (i + 1)) xs) ++ (take (i * indentSpace) (repeat ' ')) ++ "}"


type Index = Int

parseJson :: JsonInput -> Value
parseJson jsonInput =
  let
    parseObjectAttribute :: JsonInput -> Index -> (ObjectAttribute, Index)
    parseObjectAttribute input i = trace ("DEBUG: index is: " ++ show i) (let
      (name, nextIndex) = parseString input i
      (value, newIndex) = trace ("DEBUG: nextIndex = " ++ show nextIndex) (nextValue input (nextIndex))
      in ((ObjectAttribute name value), newIndex))
    nextValue :: JsonInput -> Index -> (Value, Index)
    nextValue input index = let
      indexChar = input !! index
      in if indexChar `elem` [' ', ',', ':', '\n', ';']
         then nextValue jsonInput (index + 1)
         else case indexChar of
                '"' -> let
                  (value, newIndex) = parseString input index
                  in (StringValue value, newIndex)
                'n' -> (NullValue, index + 4)
                'f' -> (BoolValue False, index + 5)
                't' -> (BoolValue True, index + 4)
                '{' -> let
                  parseObjectValue :: JsonInput -> Index -> [ObjectAttribute] -> ([ObjectAttribute], Index)
                  parseObjectValue input1 i attrs = let
                    indexChar = input1 !! i
                    in case indexChar of
                      ' ' -> parseObjectValue input1 (i + 1) attrs
                      '\n' -> parseObjectValue input1 (i + 1) attrs
                      ',' -> parseObjectValue input1 (i + 1) attrs
                      '}' -> (attrs, i + 1)
                      _   -> let
                        (newAttr, newIndex) = trace ("DEBUG: index parsed to parse objec attribute: " ++ show i) (parseObjectAttribute input1 i)
                        in parseObjectValue input1 newIndex (attrs ++ [newAttr])
                    in let
                      (attrs, newIndex) = parseObjectValue input (index + 1) []
                      in (ObjectValue attrs, newIndex)
                '[' -> let
                  parseList :: JsonInput -> Index -> [Value] -> ([Value], Index)
                  parseList input1 i values = let
                    indexChar = input1 !! i
                    in case indexChar of
                      ' ' -> parseList input1 (i + 1) values
                      ',' -> parseList input1 (i + 1) values
                      '\n' -> parseList input1 (i + 1) values
                      ']' -> (values, i + 1)
                      _   -> let
                        (value2, index2) = nextValue input1 i
                        in parseList input1 index2  (values ++ [value2])
                    in let
                      (values3, index3) = parseList input (index + 1) []
                      in (ArrayValue values3, index3)
                c -> if isDigit c
                  then let
                    (number, newIndex) = parseInt input index
                    in (IntValue number, newIndex)
                  else (StringValue ("unknown character to parse: " ++ [c]), index + 1)
  in fst (nextValue jsonInput 0)

-- should be extended for float parsing
parseInt :: JsonInput -> Index -> (Int, Index)
parseInt input i = let
  numberString = takeWhile isDigit (drop i input)
  newIndex = i + length numberString
  number = read numberString :: Int
  in (number, newIndex)


indentSpace = 4

parseString :: JsonInput -> Index -> (String, Index)
parseString input i = let
  startAtIndex = snd (splitAt (i + 1) input)
  value = takeWhile (/= '\"') startAtIndex
  in trace ("DEBUG: parse string index: " ++ show (length value + i + 2)) (value, length value + i + 2)
