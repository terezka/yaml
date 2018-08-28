module Yaml.Parser exposing (Value(..), Property, parser)

import Parser exposing (..)



-- AST


{-| -}
type Value
  = String_ String
  | Float_ Float
  | Int_ Int
  | List_ (List Value)
  | Record_ (List Property)


{-| -}
type alias Property =
  { name : String 
  , value : Value 
  }



-- PARSER


{-| -}
parser : Parser Value
parser =
  succeed identity
    |. documentBegins
    |= yamlValueTopLevel
    |. documentEnds



-- DOCUMENT / BEGINS


documentBegins : Parser ()
documentBegins =
  oneOf 
    [ whitespace |. threeDashesAndTrash |. whitespace
    , whitespace
    ]


threeDashesAndTrash : Parser ()
threeDashesAndTrash =
  symbol "---"
    |. chompUntilEndOr "\n"



-- DOCUMENT / ENDS


documentEnds : Parser (a -> a)
documentEnds =
  succeed identity
    |. whitespace
    |. end



-- YAML / VALUE


yamlValueTopLevel : Parser Value
yamlValueTopLevel =
  oneOf
    [ yamlNumber
    , yamlString
    ]


yamlString : Parser Value
yamlString =
  succeed ()
    |. chompUntilEndOr "\n"
    |> mapChompedString (\string _ -> String_ string)


yamlNumber : Parser Value
yamlNumber =
  number
    { int = Just Int_
    , hex = Just Int_ 
    , octal = Nothing 
    , binary = Nothing
    , float = Just Float_
    }



-- COMMON


whitespace : Parser ()
whitespace =
  chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

