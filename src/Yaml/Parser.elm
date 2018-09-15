module Yaml.Parser exposing (Value, toString, parser, run)

import Parser exposing (..)
import Yaml.Parser.Ast as Ast
import Yaml.Parser.Util as U
import Yaml.Parser.Document
import Yaml.Parser.String


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
    |= andThen yamlValue Yaml.Parser.Document.begins -- TODO move get indent up to here
    |. Yaml.Parser.Document.ends



-- YAML / VALUE


yamlValue : Int -> Parser Ast.Value
yamlValue indent =
  oneOf
    [ yamlRecordInline
    , yamlListInline
    , yamlList indent
    , yamlRecord True indent
    , yamlNull
    , Yaml.Parser.String.toplevel
    ]


yamlValueInline : List Char -> Parser Ast.Value
yamlValueInline endings =
  oneOf
    [ yamlRecordInline
    , yamlListInline
    , Yaml.Parser.String.inline endings
    ]



-- YAML / NULL


yamlNull : Parser Ast.Value
yamlNull =
  succeed Ast.Null_
    |. U.newLine



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
  U.checkIndent indent
    { smaller = succeed finish
    , exactly = oneOf [ map next yamlListNewEntry, succeed finish ]
    , larger  = map continued << yamlListContinuedEntry values
    , ending = succeed finish
    }


yamlListNewEntry : Parser Ast.Value
yamlListNewEntry =
  succeed identity
    |. U.dash
    |= oneOf 
        [ succeed Ast.Null_
            |. U.newLine
        , succeed identity
            |. U.space
            |. U.spaces
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
          |= Yaml.Parser.String.inline ['\n']
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
          |= Yaml.Parser.String.inline ['\n']
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
  U.propertyName first |> andThen withProperty


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
  U.checkIndent indent
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
                  |. U.space
                  |. U.spaces
                  |= yamlRecordValueInline
              ]

        Err _ -> 
          yamlRecordMissingColon
  in
  U.propertyName False |> andThen withProperty


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
        |= Yaml.Parser.String.inline ['\n']
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
          |= Yaml.Parser.String.inline ['\n']
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
    |. U.spaces
    |= oneOf
        [ succeed []
            |. symbol "}"
        , loop [] yamlListInlineEach
        ]


yamlListInlineEach : List Ast.Value -> Parser (Step (List Ast.Value) (List Ast.Value))
yamlListInlineEach values =
  succeed (\v next -> next (v :: values))
    |= yamlValueInline [',', ']']
    |. U.spaces
    |= oneOf
        [ succeed Loop |. symbol "," 
        , succeed (Done << List.reverse) |. symbol "]"
        ]
    |. U.spaces



-- YAML / RECORD / INLINE


yamlRecordInline : Parser Ast.Value
yamlRecordInline =
  succeed Ast.Record_
    |. symbol "{"
    |. U.spaces
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
                [ succeed Loop |. U.comma
                , succeed (Done << List.reverse) |. symbol "}"
                ]
            |. U.spaces

        Err _ -> 
          errorMissingColon
  in
  U.propertyName False |> andThen withProperty


yamlRecordInlineValue : Parser Ast.Value
yamlRecordInlineValue =
  oneOf
    [ succeed identity
        |. oneOf [ U.space, U.newLine ]
        |. U.whitespace -- TODO ?
        |= yamlValueInline [',', '}']
        |. U.spaces
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


