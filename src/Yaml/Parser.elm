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
  | Null_


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
    |= andThen yamlValue documentBegins
    |. documentEnds



-- DOCUMENT / BEGINS


documentBegins : Parser Int
documentBegins =
  oneOf 
    [ succeed identity
        |. spaces 
        |. threeDashesAndTrash 
        |= nextIndent
    , succeed identity
        |= nextIndent
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


yamlValue : Int -> Parser Value
yamlValue indent =
  oneOf
    [ yamlList indent
    , yamlRecordInline
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



-- YAML / LIST


yamlList : Int -> Parser Value
yamlList indent =
  succeed (\e r -> List_ (r ++ [ e ]))
    |= yamlListOne
    |= loop [] (yamlListNext indent)


yamlListNext : Int -> List Value -> Parser (Step (List Value) (List Value))
yamlListNext indent values =
  withIndent indent 
    { ok = 
        succeed identity
          |= oneOf
              [ succeed (\i -> Loop (i :: values)) |= yamlListOne
              , succeed (Done values) -- TODO correct order
              ]
    , err = succeed (Done (List.reverse values))
    }


yamlListOne : Parser Value
yamlListOne =
  succeed identity
    |. symbol "- "
    |. actualSpaces
    |= oneOf
        [ succeed identity 
            |. symbol "\n"
            |. actualSpaces
            |= andThen yamlValue getCol
        , yamlValueInline ['\n']
        ]



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
    |= yamlValueInline ['\n', ',', ']']
    |. actualSpaces
    |= oneOf
        [ succeed Loop
            |. symbol "," 
        , succeed (Done << List.reverse)
            |. symbol "]"
        , succeed ()
            |. symbol "\n"
            |> andThen (\_ -> problem "An inline record must only be on one line.")
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
        , succeed ()
            |. symbol "\n"
            |> andThen (\_ -> problem "An inline record must only be on one line.")
        ]
    |. actualSpaces



-- COMMON


stringUntil : List Char -> Parser String
stringUntil endings =
  succeed ()
    |. chompWhile (\c -> not (List.member c endings))
    |> getChompedString


actualSpaces : Parser ()
actualSpaces =
  chompWhile (\c -> c == ' ')


newLines : Parser ()
newLines =
  chompWhile (\c -> c == '\n')


newLine : Parser ()
newLine =
  chompIf (\c -> c == '\n')


nextIndent : Parser Int
nextIndent =
  loop 0 nextIndentHelp


nextIndentHelp : Int -> Parser (Step Int Int)
nextIndentHelp _ =
  succeed (\i next -> next i)
    |. actualSpaces
    |= getCol
    |= oneOf 
        [ succeed Loop |. newLine
        , succeed Done
        ]


withIndent : Int -> { ok : Parser a, err : Parser a } -> Parser a
withIndent indent next =
  let check actual =
        if actual == indent 
          then next.ok
          else next.err
  in
  andThen check nextIndent

