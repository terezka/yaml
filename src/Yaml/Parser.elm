module Yaml.Parser exposing (Value, toString, parser, fromString)

import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Ast as Ast
import Yaml.Parser.Util as U
import Yaml.Parser.Document
import Yaml.Parser.String
import Yaml.Parser.Null
import Dict


{-| -}
type alias Value =
  Ast.Value


{-| -}
toString : Value -> String
toString =
  Ast.toString



-- PARSER


{-| -}
fromString : String -> Result (List P.DeadEnd) Ast.Value
fromString =
  P.run parser


{-| -}
parser : P.Parser Ast.Value
parser =
  P.succeed identity
    |. Yaml.Parser.Document.begins
    |= value
    |. Yaml.Parser.Document.ends



-- YAML / VALUE


value : P.Parser Ast.Value
value =
  P.oneOf
    [ Yaml.Parser.String.exceptions
    , recordInline
    , listInline
    , P.andThen listToplevel U.nextIndent
    , P.andThen (recordToplevel True) U.nextIndent
    , Yaml.Parser.String.toplevel
    ]


valueToplevel : P.Parser Ast.Value
valueToplevel =
  P.lazy <| \_ -> 
    P.oneOf
      [ Yaml.Parser.String.exceptions
      , recordInline
      , listInline
      , P.andThen listToplevel P.getCol
      , P.andThen (recordToplevel False) P.getCol
      , Yaml.Parser.Null.inline
      , Yaml.Parser.String.inline ['\n']
      ]


valueInline : List Char -> P.Parser Ast.Value
valueInline endings =
  P.lazy <| \_ -> 
    P.oneOf
      [ recordInline
      , listInline
      , Yaml.Parser.String.inline endings
      ]



-- LIST / TOP LEVEL


{-| -}
listToplevel : Int -> P.Parser Ast.Value
listToplevel indent =
  let
    withValue value_ =
      P.succeed Ast.List_
        |= P.loop [ value_ ] (listToplevelEach indent)
  in
  listToplevelNewEntry
    |> P.andThen withValue


listToplevelEach : Int -> List Ast.Value -> P.Parser (P.Step (List Ast.Value) (List Ast.Value))
listToplevelEach indent values =
  let finish = P.Done (List.reverse values)
      next value_ = P.Loop (value_ :: values)
      continued = P.Loop
  in
  U.checkIndent indent
    { smaller = P.succeed finish
    , exactly = P.oneOf [ P.map next (listToplevelNewEntry), P.succeed finish ]
    , larger  = P.map continued << listToplevelContinuedEntry values
    , ending = P.succeed finish
    }


listToplevelNewEntry : P.Parser Ast.Value
listToplevelNewEntry =
  P.succeed identity
    |. U.dash
    |= P.oneOf 
        [ P.succeed Ast.Null_ -- TODO
            |. U.newLine
        , P.succeed identity
            |. U.space
            |. U.spaces
            |= valueToplevel
        ]


listToplevelContinuedEntry : List Ast.Value -> Int -> P.Parser (List Ast.Value)
listToplevelContinuedEntry values subIndent =
  let
    coalesce value_ =
      case ( values, value_ ) of
        ( Ast.Null_ :: rest, _ ) -> 
          P.succeed (value_ :: rest)

        ( Ast.String_ prev :: rest, Ast.String_ new ) -> -- TODO don't skip new lines
          P.succeed (Ast.String_ (prev ++ " " ++ new) :: rest)

        ( _, _ ) -> 
          P.problem "I was parsing a list, but I got something unexpected when expecting a new entry!"
  in
  P.andThen coalesce valueToplevel



-- LIST / INLINE


{-| -}
listInline : P.Parser Ast.Value
listInline =
  P.succeed Ast.List_
    |. P.chompIf U.isListStart
    |. U.whitespace
    |= listInlineStepOne


listInlineStepOne : P.Parser (List Ast.Value)
listInlineStepOne =
  P.oneOf 
    [ P.succeed [] 
        |. P.chompIf U.isListEnd
    , P.succeed identity
        |= P.loop [] listInlineStep
    ]


listInlineStep : List Ast.Value -> P.Parser (P.Step (List Ast.Value) (List Ast.Value))
listInlineStep elements =
  P.succeed identity
    |. U.whitespace
    |= listInlineValue
    |> P.andThen (listInlineNext elements)


listInlineValue : P.Parser Ast.Value
listInlineValue =
  P.oneOf
    [ listInline
    , recordInline
    , listInlineString
    ]


listInlineString : P.Parser Ast.Value
listInlineString =
  P.succeed ()
    |. P.chompWhile (U.neither U.isComma U.isListEnd)
    |> P.getChompedString
    |> P.map Ast.fromString


listInlineNext : List Ast.Value -> Ast.Value -> P.Parser (P.Step (List Ast.Value) (List Ast.Value))
listInlineNext elements element =
  P.oneOf
    [ P.succeed (listInlineOnMore elements element)
        |. P.chompIf U.isComma
    , P.succeed (listInlineOnDone elements element)
        |. P.chompIf U.isListEnd
    ]


listInlineOnMore : List Ast.Value -> Ast.Value -> P.Step (List Ast.Value) (List Ast.Value)
listInlineOnMore elements element =
  element :: elements
    |> P.Loop


listInlineOnDone : List Ast.Value -> Ast.Value -> P.Step (List Ast.Value) (List Ast.Value)
listInlineOnDone elements element =
  element :: elements
    |> List.reverse
    |> P.Done




-- RECORD / TOPLEVEL


{-| -}
recordToplevel : Bool -> Int -> P.Parser Ast.Value
recordToplevel isFirstValue indent =
  let
    withProperty name =
      case name of 
        Ok validName -> recordToplevelConfirmed indent validName
        Err value_ -> P.succeed value_
  in
  U.propertyName isFirstValue 
    |> P.andThen withProperty


recordToplevelConfirmed : Int -> String -> P.Parser Ast.Value
recordToplevelConfirmed indent name =
  let
    withValue value_ =
      P.succeed (Ast.Record_ << Dict.fromList)
        |= P.loop [ Tuple.pair name value_ ] (recordToplevelEach indent)
  in
  valueInline ['\n']
    |> P.andThen withValue


recordToplevelEach : Int -> List Ast.Property -> P.Parser (P.Step (List Ast.Property) (List Ast.Property))
recordToplevelEach indent properties =
  let finish = P.Done (List.reverse properties)
      next property = P.Loop (property :: properties)
      continued = P.Loop

      recordToplevelNext =
        P.oneOf 
          [ P.map next recordToplevelNewEntry
          , P.succeed finish 
          ]

      recordToplevelNextOrList ( name, _ ) rest =
        P.oneOf 
          [ P.map (\list -> continued (( name, list ) :: rest)) (listToplevel indent)
          , P.map next recordToplevelNewEntry
          , P.succeed finish 
          ]
  in
  U.checkIndent indent
    { smaller = P.succeed finish
    , exactly = 
        case properties of 
          ( name, value_ ) :: rest ->
            case value_ of
              Ast.Null_ ->
                -- Lists are allowed on the same level as the record
                recordToplevelNextOrList ( name, value_ ) rest
                
              _ ->
                recordToplevelNext

          _ ->
            recordToplevelNext
    , larger = P.map continued << recordToplevelContinuedEntry properties
    , ending = P.succeed finish
    }


recordToplevelNewEntry : P.Parser Ast.Property
recordToplevelNewEntry =
  let
    withProperty name =
      case name of 
        Ok validName ->
          P.map (Tuple.pair validName) <|
            P.oneOf 
              [ Yaml.Parser.Null.inline
              , P.succeed identity
                  |. U.space
                  |. U.spaces
                  |= valueInline ['\n']
              ]

        Err _ -> 
          recordToplevelMissingColon
  in
  U.propertyName False |> P.andThen withProperty


recordToplevelContinuedEntry : List Ast.Property -> Int -> P.Parser (List Ast.Property)
recordToplevelContinuedEntry properties subIndent =
  let
    coalesce new =
      case properties of
        ( name, value_ ) :: rest ->
          case ( value_, new ) of
            ( Ast.Null_, _ ) -> 
              P.succeed (( name, new ) :: rest)

            ( Ast.String_ prev, Ast.String_ string ) -> -- TODO don't skip new lines
              P.succeed (( name, Ast.String_ (prev ++ " " ++ string) ) :: rest)

            ( _, _ ) ->
              recordToplevelMissingProperty new

        rest ->
          recordToplevelMissingProperty new
  in
  P.andThen coalesce valueToplevel


recordToplevelMissingColon : P.Parser a
recordToplevelMissingColon =
  P.problem "I was parsing a record, but I couldn't find the \":\"!"


recordToplevelMissingProperty : Ast.Value -> P.Parser a
recordToplevelMissingProperty value_ =
  P.problem <|
    "I was parsing a record and was expecting a new property, but instead I got " ++
      case value_ of
        Ast.Null_ -> "a null"
        Ast.String_ string -> "a string (" ++ string ++ ")"
        Ast.Record_ record -> "another record!"
        Ast.List_ list -> "a list!"
        Ast.Int_ list -> "an int!"
        Ast.Float_ list -> "a float!"
        Ast.Bool_ _ -> "a boolean!"



-- RECORD / INLINE


{-| -}
recordInline : P.Parser Ast.Value
recordInline =
  P.succeed (Ast.Record_ << Dict.fromList)
    |. P.symbol "{"
    |. U.whitespace
    |= P.oneOf
        [ P.succeed [] |. P.symbol "}"
        , P.loop [] recordInlineEach
        ]


recordInlineEach : List Ast.Property -> P.Parser (P.Step (List Ast.Property) (List Ast.Property))
recordInlineEach properties =
  let
    withProperty name =
      case name of 
        Ok validName ->
          P.succeed (\v next -> next (Tuple.pair validName v :: properties))
            |= recordInlineValue
            |. U.whitespace
            |= P.oneOf
                [ P.succeed P.Loop |. U.comma |. U.whitespace
                , P.succeed (P.Done << List.reverse) |. P.symbol "}" |. U.spaces
                ]

        Err _ -> 
          errorMissingColon
  in
  U.propertyName False 
    |> P.andThen withProperty


recordInlineValue : P.Parser Ast.Value
recordInlineValue =
  P.oneOf
    [ P.succeed identity
        |. P.oneOf [ U.space, U.newLine ]
        |. U.whitespace
        |= valueInline [',', '}']
        |. U.spaces
    , P.succeed ()
        |. P.chompIf (\c -> c /= ',' && c /= '}' && c /= '\n')
        |> P.andThen (\_ -> errorMissingSpaceAfterColon)
    , P.succeed Ast.Null_
    ]


errorMissingColon : P.Parser a 
errorMissingColon =
  P.problem "I was parsing an inline record, but I couldn't find the \":\"!"


errorMissingSpaceAfterColon : P.Parser a 
errorMissingSpaceAfterColon =
  P.problem "I was parsing an inline record, but missing a space between the \":\" and the value!"

