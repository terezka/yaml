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
    , P.andThen list U.nextIndent
    , P.andThen recordToplevel U.nextIndent
    , Yaml.Parser.String.toplevel
    ]


valueToplevel : P.Parser Ast.Value
valueToplevel =
  P.lazy <| \_ -> 
    P.oneOf
      [ Yaml.Parser.String.exceptions
      , recordInline
      , listInline
      , P.andThen list P.getCol
      , P.andThen recordToplevelInner P.getCol
          |> P.andThen (\result ->
              case result of
                Ok value_ -> P.succeed value_
                Err string -> 
                  P.succeed (\r -> Ast.fromString (string ++ r)) 
                  |= U.remaining
              )
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


list : Int -> P.Parser Ast.Value
list indent =
  let
    confirmed value_ =
      P.succeed Ast.List_
        |= P.loop [ value_ ] (listStep indent)
  in
  listElement indent
    |> P.andThen confirmed
  

listStep : Int -> List Ast.Value -> P.Parser (P.Step (List Ast.Value) (List Ast.Value))
listStep indent values =
  let finish = P.Done (List.reverse values)
      next value_ = P.Loop (value_ :: values)
  in
  U.indented indent
    { smaller = 
        P.succeed finish
    , exactly = 
        P.succeed next
          |= listElement indent
    , larger = \_ -> 
        P.problem "I was looking for the next element but didn't find one."
    , ending = 
        P.succeed finish
    }


listElement : Int -> P.Parser Ast.Value
listElement indent =
  P.succeed identity 
    |. listElementBegin
    |= U.indented indent
        { smaller = 
            P.succeed Ast.Null_
        , exactly =
            P.succeed Ast.Null_
        , larger = \_ ->
            listElementValue indent 
        , ending = 
            P.succeed Ast.Null_
        }


listElementBegin : P.Parser ()
listElementBegin = 
  P.oneOf 
    [ P.symbol "- "
    , P.symbol "-\n"
    ]


listElementValue : Int -> P.Parser Ast.Value
listElementValue indent =
  P.oneOf
    [ listInline
    , recordInline
    , P.andThen list P.getCol
    , P.andThen (listRecordOrString indent) P.getCol
    ]


-- TODO move down
listRecordOrString : Int -> Int -> P.Parser Ast.Value
listRecordOrString indent indent_ =
  let
    withQuote qoute =
      P.oneOf
        [ property qoute
        , P.succeed (Ast.String_ qoute)
        ]

    withString string =
      P.oneOf
        [ property string
        , P.succeed (addRemaining string)
            |= U.multiline indent
        ]

    property name =
      P.succeed (recordToplevelConfirmed indent_ name)
        |. P.chompIf U.isColon 
        |> P.andThen identity

    addRemaining string remaining =
      Ast.fromString (string ++ remaining)
  in
  P.oneOf
    [ P.succeed identity
        |= P.oneOf [ U.singleQuotes, U.doubleQuotes ]
        |. U.spaces
        |> P.andThen withQuote
    , P.succeed identity
        |. P.chompWhile (U.neither U.isColon U.isNewLine)
        |> P.getChompedString
        |> P.andThen withString
    ]



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
    |. U.whitespace
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
recordToplevel : Int -> P.Parser Ast.Value
recordToplevel indent =
  P.oneOf 
    [ P.andThen (fromQuotedPropertyName indent) U.singleQuotes
        |> P.andThen (\r -> 
            case r of
              Ok v -> P.succeed v
              Err s -> P.succeed (Ast.String_ s)
          )
    , P.andThen (fromQuotedPropertyName indent) U.doubleQuotes
        |> P.andThen (\r -> 
            case r of
              Ok v -> P.succeed v
              Err s -> P.succeed (Ast.String_ s)
          )
    , U.fork
        [ U.Branch (P.symbol ":") (recordToplevelConfirmed indent)
        , U.Branch Yaml.Parser.Document.ending (P.succeed << Ast.fromString)
        ]
    ]


{-| -}
recordToplevelInner : Int -> P.Parser (Result String Ast.Value)
recordToplevelInner indent =
  P.oneOf 
    [ P.andThen (fromQuotedPropertyName indent) U.singleQuotes
    , P.andThen (fromQuotedPropertyName indent) U.doubleQuotes
    , U.fork
        [ U.Branch (P.symbol ":") (\v -> P.succeed Ok |= recordToplevelConfirmed indent v)
        , U.Branch (P.symbol "\n") (P.succeed << Err)
        , U.Branch Yaml.Parser.Document.ending (P.succeed << Err)
        ]
    ]


fromQuotedPropertyName : Int -> String -> P.Parser (Result String Ast.Value)
fromQuotedPropertyName indent name =
  P.succeed identity
    |. U.spaces
    |= P.oneOf
        [ P.succeed ()
            |. P.chompIf U.isColon
            |> P.andThen (\_ -> P.succeed Ok |= recordToplevelConfirmed indent name)
        , P.succeed (Err name)
            |. U.whitespace
            |. P.end
        ]


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
          [ P.map (\value_ -> continued (( name, value_ ) :: rest)) (list indent)
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
        Ast.List_ _ -> "a list!"
        Ast.Int_ _ -> "an int!"
        Ast.Float_ _ -> "a float!"
        Ast.Bool_ _ -> "a boolean!"



-- RECORD / INLINE


{-| -}
recordInline : P.Parser Ast.Value
recordInline =
  P.succeed (Ast.Record_ << Dict.fromList)
    |. P.chompIf U.isRecordStart
    |. U.whitespace
    |= recordInlineStepOne


recordInlineStepOne : P.Parser (List Ast.Property)
recordInlineStepOne =
  P.oneOf 
    [ P.succeed [] 
        |. P.chompIf U.isRecordEnd
    , P.succeed identity
        |= P.loop [] recordInlineStep
    ]


recordInlineStep : List Ast.Property -> P.Parser (P.Step (List Ast.Property) (List Ast.Property))
recordInlineStep elements =
  P.succeed identity
    |. U.whitespace
    |= recordInlineValue
    |. U.whitespace
    |> P.andThen (recordInlineNext elements)


recordInlineValue : P.Parser Ast.Property
recordInlineValue =
  P.succeed Tuple.pair
    |= recordInlinePropertyName
    |. U.whitespace
    |= recordInlinePropertyValue


recordInlinePropertyName : P.Parser String
recordInlinePropertyName =
  P.succeed identity
    |= P.oneOf
        [ U.singleQuotes
        , U.doubleQuotes
        , recordInlinePropertyNameString
        ]
    |. P.chompWhile U.isSpace
    |. P.oneOf
        [ P.chompIf U.isColon
        , P.problem "I was parsing an inline record, when I ran into an invalid property. It is missing the \":\"!"
        ]
    |. P.oneOf
        [ P.chompIf U.isNewLine
        , P.chompIf U.isSpace
        , P.problem "I was parsing an inline record, but missing a space or a new line between the \":\" and the value!"
        ]


recordInlinePropertyNameString : P.Parser String
recordInlinePropertyNameString = -- TODO allow numeric name
  P.succeed ()
    |. P.chompWhile (U.neither3 U.isColon U.isComma U.isRecordEnd)
    |> P.getChompedString
    |> P.map String.trim


recordInlinePropertyValue : P.Parser Ast.Value
recordInlinePropertyValue =
  P.oneOf
    [ listInline
    , recordInline
    , recordInlineString
    ]


recordInlineString : P.Parser Ast.Value
recordInlineString =
  P.succeed ()
    |. P.chompWhile (U.neither U.isComma U.isRecordEnd)
    |> P.getChompedString
    |> P.map Ast.fromString


recordInlineNext : List Ast.Property -> Ast.Property -> P.Parser (P.Step (List Ast.Property) (List Ast.Property))
recordInlineNext elements element =
  P.oneOf
    [ P.succeed (recordInlineOnMore elements element)
        |. P.chompIf U.isComma
    , P.succeed (recordInlineOnDone elements element)
        |. P.chompIf U.isRecordEnd
    ]


recordInlineOnMore : List Ast.Property -> Ast.Property -> P.Step (List Ast.Property) (List Ast.Property)
recordInlineOnMore elements element =
  element :: elements
    |> P.Loop


recordInlineOnDone : List Ast.Property -> Ast.Property -> P.Step (List Ast.Property) (List Ast.Property)
recordInlineOnDone elements element =
  element :: elements
    |> List.reverse
    |> P.Done
