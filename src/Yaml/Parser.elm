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
    [ spaces |. threeDashesAndTrash |. spaces
    , spaces
    ]


threeDashesAndTrash : Parser ()
threeDashesAndTrash =
  symbol "---"
    |. chompUntilEndOr "\n"



-- DOCUMENT / ENDS


documentEnds : Parser (a -> a)
documentEnds =
  succeed identity
    |. spaces
    |. end



-- YAML / VALUE


yamlValueTopLevel : Parser Value
yamlValueTopLevel =
  oneOf
    [ yamlListInline
    , yamlNumber
    , yamlString
    ]


yamlValueInline : List Char -> Parser Value
yamlValueInline endings =
  oneOf
    [ yamlListInline
    , yamlNumber
    , yamlStringUntil endings
    ]



-- YAML / STRING


yamlString : Parser Value
yamlString =
  yamlStringUntil ['\n']


yamlStringUntil : List Char -> Parser Value
yamlStringUntil endings =
  succeed ()
    |. chompWhile (\c -> not (List.member c endings))
    |> mapChompedString (\string _ -> String_ string)



-- YAML / NUMBER


yamlNumber : Parser Value
yamlNumber =
  number
    { int = Just Int_
    , hex = Just Int_ 
    , octal = Nothing 
    , binary = Nothing
    , float = Just Float_
    }



-- YAML / LIST / INLINE


yamlListInline : Parser Value
yamlListInline =
  succeed List_
    |. symbol "["
    |. spaces
    |= loop [] yamlListInlineEach


yamlListInlineEach : List Value -> Parser (Step (List Value) (List Value))
yamlListInlineEach values =
  succeed (\v next -> next (v :: values))
    |= yamlValueInline [',', ']', '\n']
    |. actualSpaces
    |= oneOf
        [ succeed Loop
            |. symbol "," 
        , succeed (Done << List.reverse)
            |. symbol "]"
        , succeed (Done << List.reverse) -- TODO When Parser.Advanced is available error
            |. chompIf (\c -> c == '\n')
        ]
    |. actualSpaces



-- COMMON


actualSpaces : Parser ()
actualSpaces =
  chompWhile (\c -> c == ' ')

