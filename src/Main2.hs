module Main2 where

import Data.List.Split

data Value = NullValue | IntValue Int | FloatValue Float | BoolValue Bool | StringValue String | ArrayValue [Value] | ObjectValue [ObjectAttribute]

data ObjectAttribute = ObjectAttribute {
  name :: String,
  value :: Value
}

type JsonInput = String

showValue v = case v of
  StringValue a -> a
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
                  value = (StringValue $ (splitOn "\"" input) !! 0)
                  in (value, index)
                _ -> (StringValue "bla", 0)
  in fst (nextValue jsonInput 0)
