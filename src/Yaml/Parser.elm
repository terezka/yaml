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
    [ yamlRecordInline
    , yamlListInline
    , yamlNumber
    , yamlString
    ]


yamlValueInline : List Char -> Parser Value
yamlValueInline endings =
  oneOf
    [ yamlRecordInline
    , yamlListInline
    , yamlNumber
    , yamlStringUntil endings
    ]



-- YAML / STRING


yamlString : Parser Value
yamlString =
  yamlStringUntil ['\n']


yamlStringUntil : List Char -> Parser Value
yamlStringUntil endings =
  succeed String_ -- TODO trim?
    |= stringUntil endings



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
    |. actualSpaces
    |= oneOf
        [ succeed []
            |. symbol "}"
        , loop [] yamlListInlineEach
        ]


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




-- YAML / RECORD / INLINE


yamlRecordInline : Parser Value
yamlRecordInline =
  succeed Record_
    |. symbol "{"
    |. actualSpaces
    |= oneOf
        [ succeed []
            |. symbol "}"
        , loop [] yamlRecordInlineEach
        ]


yamlRecordInlineEach : List Property -> Parser (Step (List Property) (List Property))
yamlRecordInlineEach properties =
  succeed (\n v next -> next (Property n v :: properties))
    |= stringUntil [':']
    |. symbol ":"
    |. actualSpaces
    |= yamlValueInline [',', '}', '\n']
    |. actualSpaces
    |= oneOf
        [ succeed Loop
            |. symbol ","
        , succeed (Done << List.reverse)
            |. symbol "}"
        , succeed (Done << List.reverse) -- TODO When Parser.Advanced is available error
            |. symbol "\n"
        ]
    |. actualSpaces



-- COMMON


actualSpaces : Parser ()
actualSpaces =
  chompWhile (\c -> c == ' ')


stringUntil : List Char -> Parser String
stringUntil endings =
  succeed ()
    |. chompWhile (\c -> not (List.member c endings))
    |> getChompedString