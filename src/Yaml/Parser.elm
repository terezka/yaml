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
  succeed identity
    |. spaces
    |= oneOf 
        [ succeed identity
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
    [ yamlRecordInline
    , yamlListInline
    , yamlList indent
    , yamlRecord indent
    , yamlString
    ]


yamlValueInline : List Char -> Parser Value
yamlValueInline endings =
  oneOf
    [ yamlRecordInline
    , yamlListInline
    , yamlStringUntil endings -- TODO do not accept empty string
    ]



-- YAML / STRING


yamlString : Parser Value
yamlString =
  yamlStringUntil ['\n']


yamlStringUntil : List Char -> Parser Value
yamlStringUntil endings =
  succeed String_
    |= oneOf
        [ succeed identity
            |= singleQuotes
            |. anyOf endings
        , succeed identity
            |= doubleQuotes
            |. anyOf endings
        , succeed String.trim
            |= stringUntil endings
        ]



-- YAML / LIST


yamlList : Int -> Parser Value
yamlList indent =
  succeed (\e r -> List_ (e :: r))
    |= yamlListNewEntry
    |= loop [] (yamlListEach indent)


yamlListEach : Int -> List Value -> Parser (Step (List Value) (List Value))
yamlListEach indent values =
  let finish = Done (List.reverse values)
      next value = Loop (value :: values)
      continued = Loop
  in
  checkIndent indent
    { smaller = succeed finish
    , exactly = oneOf [ map next yamlListNewEntry, succeed finish ]
    , larger  = map continued << yamlListContinuedEntry values
    , ending = succeed finish
    }


yamlListNewEntry : Parser Value
yamlListNewEntry =
  succeed identity
    |. singleDash
    |= oneOf 
        [ succeed Null_
            |. newLine
        , succeed identity
            |. singleSpace
            |. manySpaces
            |= oneOf
                [ yamlListInline
                , yamlRecordInline
                , succeed Null_ 
                    |. newLine
                , succeed String_ 
                    |= singleQuotes
                    |. newLine
                , succeed String_ 
                    |= doubleQuotes
                    |. newLine
                , succeed String_ 
                    |= lineOfCharacters
                    |. newLine
                ]
        ]


yamlListContinuedEntry : List Value -> Int -> Parser (List Value)
yamlListContinuedEntry values subIndent =
  let
    coalesce value =
      case ( values, value ) of
        ( Null_ :: rest, _ ) -> 
          succeed (value :: rest)

        ( String_ prev :: rest, String_ new ) -> 
          succeed (String_ (prev ++ " " ++ new) :: rest)

        ( _ :: rest, String_ _ ) -> -- TODO don't skip new lines
          problem "Expected \"-\""

        ( _, _ ) -> 
          succeed (value :: values)
  in
  andThen coalesce <|
    oneOf
      [ yamlListInline
      , yamlRecordInline
      , yamlRecord subIndent
      , yamlList subIndent
      , succeed Null_ 
          |. newLine
      , succeed String_ 
          |= singleQuotes
          |. newLine
      , succeed String_ 
          |= doubleQuotes
          |. newLine
      , succeed String_ 
          |= lineOfCharacters
          |. newLine
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
    |= yamlValueInline [',', ']']
    |. actualSpaces
    |= oneOf
        [ succeed Loop |. symbol "," 
        , succeed (Done << List.reverse) |. symbol "]"
        ]
    |. actualSpaces



-- YAML / RECORD


yamlRecord : Int -> Parser Value
yamlRecord indent =
  let
    withProperty name =
      case name of 
        Ok validName -> yamlRecordConfirmed indent validName
        Err string -> succeed (String_ string)
  in
  propertyName |> andThen withProperty 


yamlRecordConfirmed : Int -> String -> Parser Value
yamlRecordConfirmed indent name =
  succeed (\v r -> Record_ (Property name v :: r))
    |= yamlRecordOneValue
    |= loop [] (yamlRecordNext indent)


yamlRecordNext : Int -> List Property -> Parser (Step (List Property) (List Property))
yamlRecordNext indent values =
  withIndent indent 
    { ok = 
        oneOf
          [ succeed (\i -> Loop (i :: values)) |= yamlRecordOne
          , succeed (Done (List.reverse values))
          ]
    , err = succeed (Done (List.reverse values))
    }


yamlRecordOne : Parser Property
yamlRecordOne =
  let
    withProperty name =
      case name of 
        Ok validName ->
          map (Property validName) yamlRecordOneValue

        Err string -> 
          problem "I was parsing a record, but I couldn't find the \":\"!"
  in
  propertyName |> andThen withProperty


yamlRecordOneValue : Parser Value
yamlRecordOneValue =
  oneOf
    [ succeed identity
        |. symbol " "
        |. actualSpaces
        |= oneOf
            [ yamlRecordNested
            , yamlValueInline ['\n']
            ] 
    , yamlRecordNested
    ]


yamlRecordNested : Parser Value
yamlRecordNested =
  succeed identity
    |. actualSpaces
    |. symbol "\n"
    |. actualSpaces
    |= andThen yamlValue getCol



-- YAML / RECORD / INLINE


yamlRecordInline : Parser Value
yamlRecordInline =
  succeed Record_
    |. symbol "{"
    |. actualSpaces
    |= oneOf
        [ succeed [] |. symbol "}"
        , loop [] yamlRecordInlineEach
        ]


yamlRecordInlineEach : List Property -> Parser (Step (List Property) (List Property))
yamlRecordInlineEach properties =
  let
    withProperty name =
      case name of 
        Ok validName ->
          succeed (\v next -> next (Property validName v :: properties))
            |= yamlRecordInlineValue
            |= oneOf
                [ succeed Loop |. symbol ","
                , succeed (Done << List.reverse) |. symbol "}"
                ]
            |. actualSpaces

        Err string -> 
          problem "I was parsing an inline record, but I couldn't find the \":\"!"
  in
  propertyName |> andThen withProperty


yamlRecordInlineValue : Parser Value
yamlRecordInlineValue =
  succeed identity
    |. oneOf 
        [ symbol " "
        , symbol "\n"
        , problem "I was parsing an inline record, but there must be a space after the \":\"!"
        ]
    |. spaces
    |= yamlValueInline [',', '}']
    |. actualSpaces



-- COMMON


singleDash : Parser ()
singleDash =
  symbol "-"


singleSpace : Parser ()
singleSpace =
  symbol " "


manySpaces : Parser ()
manySpaces =
  chompWhile (\c -> c == ' ')


anyOf : List Char -> Parser ()
anyOf endings =
  chompIf (\c -> List.member c endings)


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


checkIndent : Int -> { smaller : Parser a, exactly : Parser a, larger : Int -> Parser a, ending : Parser a } -> Parser a
checkIndent indent next =
  let check actual =
        oneOf
          [ andThen (always next.ending) end 
          , if actual == indent then next.exactly
            else if actual > indent then next.larger actual
            else next.smaller
          ]
        
  in
  andThen check nextIndent



-- PROPERTY NAME


propertyName : Parser (Result String String)
propertyName =
  let valid = Ok
      invalid s1 s2 = Err (s1 ++ s2)
  in
  succeed apply
    |= oneOf
        [ succeed identity
            |= singleQuotes
        , succeed identity
            |= doubleQuotes
        , succeed String.trim
            |= stringUntil [':', '\n']
        ]
    |= oneOf
        [ succeed valid 
            |. anyOf [':']
        , succeed invalid
            |= stringUntil ['\n']
        ]


singleQuotes : Parser String
singleQuotes =
  succeed (String.replace "\\" "\\\\")
    |. symbol "'"
    |= stringUntil ['\'']
    |. symbol "'"
    |. actualSpaces


doubleQuotes : Parser String
doubleQuotes =
  succeed identity
    |. symbol "\""
    |= stringUntil ['"']
    |. symbol "\""
    |. actualSpaces


lineOfCharacters : Parser String
lineOfCharacters =
  succeed ()
    |. chompIf (\c -> c /= '\n')
    |. chompUntilEndOr "\n"
    |> getChompedString


apply : a -> (a -> b) -> b
apply v f =
  f v
