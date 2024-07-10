module Main2 where

data LineProperty = Start | End | Value

data SetType = Attr | List

data ListType = ListType {
  lineProperty :: LineProperty,
  listType :: ListType
}

newtype JsonLine = JsonLine String
newtype JsonLines = JsonLines [JsonLine]

newtype NixLine = NixLine String
newtype NixLines = NixLines [NixLine]

json2nix :: JsonLines -> NixLines
json2nix s = NixLines [NixLine "hello"]
