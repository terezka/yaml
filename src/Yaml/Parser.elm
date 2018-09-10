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
    , yamlNull
    , yamlString
    ]


yamlValueInline : List Char -> Parser Value
yamlValueInline endings =
  oneOf
    [ yamlRecordInline
    , yamlListInline
    , yamlStringInline endings
    ]



-- YAML / NULL


yamlNull : Parser Value
yamlNull =
  succeed Null_
    |. newLine



-- YAML / STRING


yamlString : Parser Value
yamlString =
  let
    multiline result =
      oneOf
        [ map (\i -> Loop (i :: result)) lineOfCharacters
        , succeed (Done (List.reverse result |> String.concat))
        ]
  in
  succeed String_ 
    |= oneOf
        [ singleQuotes
        , doubleQuotes
        , loop [] multiline
        ]        


yamlStringInline : List Char -> Parser Value
yamlStringInline endings =
  let
    toValue string =
      case string of
        "" -> Null_
        other -> String_ other
  in
   oneOf
    [ succeed String_
        |= singleQuotes
        |. anyOf endings
    , succeed String_
        |= doubleQuotes
        |. anyOf endings
    , succeed String.trim
        |= characters endings
        |> map toValue
    ]



-- YAML / LIST


yamlList : Int -> Parser Value
yamlList indent =
  let
    withValue value =
      succeed List_
        |= loop [ value ] (yamlListEach indent)
  in
  yamlListNewEntry
    |> andThen withValue


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
            |= yamlListValue
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
  andThen coalesce yamlListValue


yamlListValueInline : Parser Value
yamlListValueInline =
  lazy <| \_ -> 
    oneOf
      [ yamlListInline
      , yamlRecordInline
      , yamlNull
      , succeed identity 
          |= yamlStringInline ['\n']
      ]


yamlListValue : Parser Value
yamlListValue =
  lazy <| \_ -> 
    oneOf
      [ yamlListInline
      , yamlRecordInline
      , andThen yamlList getCol
      , andThen yamlRecord getCol
      , yamlNull
      , succeed identity 
          |= yamlStringInline ['\n']
      ]



-- YAML / RECORD


yamlRecord : Int -> Parser Value
yamlRecord indent =
  let
    withProperty name =
      case name of 
        Ok validName -> yamlRecordConfirmed indent validName
        Err value -> succeed value
  in
  propertyName |> andThen withProperty


yamlRecordConfirmed : Int -> String -> Parser Value
yamlRecordConfirmed indent name =
  let
    withValue value =
      succeed Record_
        |= loop [ Property name value ] (yamlRecordEach indent)
  in
  yamlRecordValueInline
    |> andThen withValue


yamlRecordEach : Int -> List Property -> Parser (Step (List Property) (List Property))
yamlRecordEach indent properties =
  let finish = Done (List.reverse properties)
      next property = Loop (property :: properties)
      continued = Loop

      yamlRecordNext =
        oneOf 
          [ map next yamlRecordNewEntry
          , succeed finish 
          ]

      yamlRecordNextOrList latest rest =
        oneOf 
          [ map (\list -> continued ({ latest | value = list } :: rest)) (yamlList indent)
          , map next yamlRecordNewEntry
          , succeed finish 
          ]
  in
  checkIndent indent
    { smaller = succeed finish
    , exactly = 
        case properties of 
          latest :: rest ->
            case latest.value of
              Null_ ->
                -- Lists are allowed on the same level as the record
                yamlRecordNextOrList latest rest
                
              _ ->
                yamlRecordNext

          _ ->
            yamlRecordNext
    , larger = map continued << yamlRecordContinuedEntry properties
    , ending = succeed finish
    }


yamlRecordNewEntry : Parser Property
yamlRecordNewEntry =
  let
    withProperty name =
      case name of 
        Ok validName ->
          map (Property validName) <|
            oneOf 
              [ yamlNull
              , succeed identity
                  |. singleSpace
                  |. manySpaces
                  |= yamlRecordValueInline
              ]

        Err _ -> 
          yamlRecordMissingColon
  in
  propertyName |> andThen withProperty


yamlRecordContinuedEntry : List Property -> Int -> Parser (List Property)
yamlRecordContinuedEntry properties subIndent =
  let
    coalesce value =
      case properties of
        latest :: rest ->
          case ( latest.value, value ) of
            ( Null_, _ ) -> 
              succeed ({ latest | value = value } :: rest)

            ( String_ prev, String_ new ) -> -- TODO don't skip new lines
              succeed ({ latest | value = String_ (prev ++ " " ++ new) } :: rest)

            ( _, _ ) ->
              yamlRecordMissingProperty value

        rest ->
          yamlRecordMissingProperty value
  in
  andThen coalesce yamlRecordValue


yamlRecordValueInline : Parser Value
yamlRecordValueInline =
  oneOf
    [ yamlListInline
    , yamlRecordInline
    , yamlNull
    , succeed identity 
        |= yamlStringInline ['\n']
    ]


yamlRecordValue : Parser Value
yamlRecordValue =
  lazy <| \_ -> 
    oneOf
      [ yamlListInline
      , yamlRecordInline
      , andThen yamlList getCol
      , andThen yamlRecord getCol
      , yamlNull
      , succeed identity 
          |= yamlStringInline ['\n']
      ]


yamlRecordMissingColon : Parser a
yamlRecordMissingColon =
  problem "I was parsing a record, but I couldn't find the \":\"!"


yamlRecordMissingProperty : Value -> Parser a
yamlRecordMissingProperty value =
  problem <|
    "I was parsing a record and was expecting a new property, but instead I got " ++
      case value of
        Null_ -> "a null"
        String_ string -> "a string (" ++ string ++ ")"
        Record_ record -> "another record!"
        List_ list -> "a list!"
        Int_ list -> "an int!"
        Float_ list -> "a float!"



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

        Err _ -> 
          errorMissingColon
  in
  propertyName |> andThen withProperty


yamlRecordInlineValue : Parser Value
yamlRecordInlineValue =
  oneOf
    [ succeed identity
        |. oneOf [ singleSpace, newLine ]
        |. spaces
        |= yamlValueInline [',', '}']
        |. actualSpaces
    , succeed ()
        |. chompIf (\c -> c /= ',' && c /= '}' && c /= '\n')
        |> andThen (\_ -> errorMissingSpaceAfterColon)
    , succeed Null_
    ]


errorMissingColon : Parser a 
errorMissingColon =
  problem "I was parsing an inline record, but I couldn't find the \":\"!"


errorMissingSpaceAfterColon : Parser a 
errorMissingSpaceAfterColon =
  problem "I was parsing an inline record, but missing a space between the \":\" and the value!"



-- COMMON


colon : Parser ()
colon =
  symbol ":"


comma : Parser ()
comma =
  symbol ","


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


characters : List Char -> Parser String
characters endings =
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


propertyName : Parser (Result Value String)
propertyName =
  let remaining =
        succeed ()
          |. chompWhile (always True)
          |> getChompedString

      valid = Ok
      invalid s2 s1 = 
        case s1 ++ s2 of
          "" -> Err Null_
          result -> Err (String_ (String.trim result))
  in
  succeed apply
    |= oneOf
        [ succeed identity
            |= singleQuotes
        , succeed identity
            |= doubleQuotes
        , succeed String.trim
            |= characters [':', '\n']
        ]
    |= oneOf
        [ succeed valid
            |. colon
        , succeed invalid
            |= characters ['\n'] -- TODO remaining
        ]


singleQuotes : Parser String
singleQuotes =
  succeed (String.replace "\\" "\\\\")
    |. symbol "'"
    |= characters ['\'']
    |. symbol "'"
    |. actualSpaces


doubleQuotes : Parser String
doubleQuotes =
  succeed identity
    |. symbol "\""
    |= characters ['"']
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
