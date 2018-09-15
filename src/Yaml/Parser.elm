module Yaml.Parser exposing (Value, toString, parser, run)

import Parser exposing (..)
import Yaml.Parser.Ast as Ast


{-| -}
type alias Value =
  Ast.Value


{-| -}
toString : Value -> String
toString =
  Ast.toString


-- PARSER


{-| -}
run : String -> Result (List Parser.DeadEnd) Ast.Value
run =
  Parser.run parser


{-| -}
parser : Parser Ast.Value
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


yamlValue : Int -> Parser Ast.Value
yamlValue indent =
  oneOf
    [ yamlRecordInline
    , yamlListInline
    , yamlList indent
    , yamlRecord True indent
    , yamlNull
    , yamlString
    ]


yamlValueInline : List Char -> Parser Ast.Value
yamlValueInline endings =
  oneOf
    [ yamlRecordInline
    , yamlListInline
    , yamlStringInline endings
    ]



-- YAML / NULL


yamlNull : Parser Ast.Value
yamlNull =
  succeed Ast.Null_
    |. newLine



-- YAML / STRING


yamlString : Parser Ast.Value
yamlString =
  let
    multiline result =
      oneOf
        [ map (\i -> Loop (i :: result)) lineOfCharacters
        , succeed (Done (List.reverse result |> String.concat))
        ]
  in
  oneOf
    [ succeed Ast.String_
        |= singleQuotes
    , succeed Ast.String_
        |= doubleQuotes
    , succeed stringToValue
        |= loop [] multiline
    ]        


yamlStringInline : List Char -> Parser Ast.Value
yamlStringInline endings =
   oneOf
    [ succeed Ast.String_
        |= singleQuotes
        |. anyOf endings
    , succeed Ast.String_
        |= doubleQuotes
        |. anyOf endings
    , succeed stringToValue
        |= characters endings
    ]



-- YAML / LIST


yamlList : Int -> Parser Ast.Value
yamlList indent =
  let
    withValue value =
      succeed Ast.List_
        |= loop [ value ] (yamlListEach indent)
  in
  yamlListNewEntry
    |> andThen withValue


yamlListEach : Int -> List Ast.Value -> Parser (Step (List Ast.Value) (List Ast.Value))
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


yamlListNewEntry : Parser Ast.Value
yamlListNewEntry =
  succeed identity
    |. singleDash
    |= oneOf 
        [ succeed Ast.Null_
            |. newLine
        , succeed identity
            |. singleSpace
            |. manySpaces
            |= yamlListValue
        ]


yamlListContinuedEntry : List Ast.Value -> Int -> Parser (List Ast.Value)
yamlListContinuedEntry values subIndent =
  let
    coalesce value =
      case ( values, value ) of
        ( Ast.Null_ :: rest, _ ) -> 
          succeed (value :: rest)

        ( Ast.String_ prev :: rest, Ast.String_ new ) -> 
          succeed (Ast.String_ (prev ++ " " ++ new) :: rest)

        ( _ :: rest, Ast.String_ _ ) -> -- TODO don't skip new lines
          problem "I was parsing a record, but I got more strings when expected a new property!"

        ( _, _ ) -> 
          succeed (value :: values)
  in
  andThen coalesce yamlListValue


yamlListValueInline : Parser Ast.Value
yamlListValueInline =
  lazy <| \_ -> 
    oneOf
      [ yamlListInline
      , yamlRecordInline
      , yamlNull
      , succeed identity 
          |= yamlStringInline ['\n']
      ]


yamlListValue : Parser Ast.Value
yamlListValue =
  lazy <| \_ -> 
    oneOf
      [ yamlListInline
      , yamlRecordInline
      , andThen yamlList getCol
      , andThen (yamlRecord False) getCol
      , yamlNull
      , succeed identity 
          |= yamlStringInline ['\n']
      ]



-- YAML / RECORD


yamlRecord : Bool -> Int -> Parser Ast.Value
yamlRecord first indent =
  let
    withProperty name =
      case name of 
        Ok validName -> yamlRecordConfirmed indent validName
        Err value -> succeed value
  in
  propertyName first |> andThen withProperty


yamlRecordConfirmed : Int -> String -> Parser Ast.Value
yamlRecordConfirmed indent name =
  let
    withValue value =
      succeed Ast.Record_
        |= loop [ Ast.Property name value ] (yamlRecordEach indent)
  in
  yamlRecordValueInline
    |> andThen withValue


yamlRecordEach : Int -> List Ast.Property -> Parser (Step (List Ast.Property) (List Ast.Property))
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
              Ast.Null_ ->
                -- Lists are allowed on the same level as the record
                yamlRecordNextOrList latest rest
                
              _ ->
                yamlRecordNext

          _ ->
            yamlRecordNext
    , larger = map continued << yamlRecordContinuedEntry properties
    , ending = succeed finish
    }


yamlRecordNewEntry : Parser Ast.Property
yamlRecordNewEntry =
  let
    withProperty name =
      case name of 
        Ok validName ->
          map (Ast.Property validName) <|
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
  propertyName False |> andThen withProperty


yamlRecordContinuedEntry : List Ast.Property -> Int -> Parser (List Ast.Property)
yamlRecordContinuedEntry properties subIndent =
  let
    coalesce value =
      case properties of
        latest :: rest ->
          case ( latest.value, value ) of
            ( Ast.Null_, _ ) -> 
              succeed ({ latest | value = value } :: rest)

            ( Ast.String_ prev, Ast.String_ new ) -> -- TODO don't skip new lines
              succeed ({ latest | value = Ast.String_ (prev ++ " " ++ new) } :: rest)

            ( _, _ ) ->
              yamlRecordMissingProperty value

        rest ->
          yamlRecordMissingProperty value
  in
  andThen coalesce yamlRecordValue


yamlRecordValueInline : Parser Ast.Value
yamlRecordValueInline =
  oneOf
    [ yamlListInline
    , yamlRecordInline
    , yamlNull
    , succeed identity 
        |= yamlStringInline ['\n']
    ]


yamlRecordValue : Parser Ast.Value
yamlRecordValue =
  lazy <| \_ -> 
    oneOf
      [ yamlListInline
      , yamlRecordInline
      , andThen yamlList getCol
      , andThen (yamlRecord False) getCol
      , yamlNull
      , succeed identity 
          |= yamlStringInline ['\n']
      ]


yamlRecordMissingColon : Parser a
yamlRecordMissingColon =
  problem "I was parsing a record, but I couldn't find the \":\"!"


yamlRecordMissingProperty : Ast.Value -> Parser a
yamlRecordMissingProperty value =
  problem <|
    "I was parsing a record and was expecting a new property, but instead I got " ++
      case value of
        Ast.Null_ -> "a null"
        Ast.String_ string -> "a string (" ++ string ++ ")"
        Ast.Record_ record -> "another record!"
        Ast.List_ list -> "a list!"
        Ast.Int_ list -> "an int!"
        Ast.Float_ list -> "a float!"



-- YAML / LIST / INLINE


yamlListInline : Parser Ast.Value
yamlListInline =
  succeed Ast.List_
    |. symbol "["
    |. actualSpaces
    |= oneOf
        [ succeed []
            |. symbol "}"
        , loop [] yamlListInlineEach
        ]


yamlListInlineEach : List Ast.Value -> Parser (Step (List Ast.Value) (List Ast.Value))
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


yamlRecordInline : Parser Ast.Value
yamlRecordInline =
  succeed Ast.Record_
    |. symbol "{"
    |. actualSpaces
    |= oneOf
        [ succeed [] |. symbol "}"
        , loop [] yamlRecordInlineEach
        ]


yamlRecordInlineEach : List Ast.Property -> Parser (Step (List Ast.Property) (List Ast.Property))
yamlRecordInlineEach properties =
  let
    withProperty name =
      case name of 
        Ok validName ->
          succeed (\v next -> next (Ast.Property validName v :: properties))
            |= yamlRecordInlineValue
            |= oneOf
                [ succeed Loop |. symbol ","
                , succeed (Done << List.reverse) |. symbol "}"
                ]
            |. actualSpaces

        Err _ -> 
          errorMissingColon
  in
  propertyName False |> andThen withProperty


yamlRecordInlineValue : Parser Ast.Value
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
    , succeed Ast.Null_
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


propertyName : Bool -> Parser (Result Ast.Value String)
propertyName first =
  let remaining =
        if first then everything else characters ['\n']

      everything =
        succeed ()
          |. chompWhile (always True)
          |> getChompedString

      valid = Ok
      invalid s2 s1 = 
        Err (stringToValue (s1 ++ s2))
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
            |= remaining
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


stringToValue : String -> Ast.Value
stringToValue string =
  case String.trim string of
    "" -> Ast.Null_
    other -> 
      case String.toInt other of
        Just int -> Ast.Int_ int
        Nothing ->
          case String.toFloat other of
            Just float -> Ast.Float_ float
            Nothing -> Ast.String_ other