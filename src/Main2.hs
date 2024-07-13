module Main2 where

data Value = NullValue | IntValue Int | FloatValue Float | BoolValue Bool | StringValue String | ArrayValue [Value] | ObjectValue [ObjectAttribute]

data ObjectAttribute = ObjectAttribute {
  name :: String,
  value :: Value
}

newtype JsonInput = JsonInput String

parseJson :: JsonInput -> Value

data ReadMode = NullMode | StringMode | IntMode | FloatMode | ObjectMode | ListMode

charToReadMode :: Char -> ReadMode
charToReadmode '{' = ObjectMode
charToReadmode '[' = ListMode
charToReadmode '"' = ListMode
charToReadmode 't' = BoolMode
charToReadmode 'f' = BoolMode
charToReadmode 'n' = NullMode

