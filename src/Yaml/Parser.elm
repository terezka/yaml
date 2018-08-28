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
    |= yamlValue
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


yamlValue : Parser Value
yamlValue =
  succeed (String_ "hi")



-- COMMON


whitespace : Parser ()
whitespace =
  chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')
