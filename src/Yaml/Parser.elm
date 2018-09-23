module Yaml.Parser exposing (Value, toString, parser, fromString)

import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Ast as Ast
import Yaml.Parser.Util as U
import Yaml.Parser.Document
import Yaml.Parser.String
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
fromString : String -> Result String Ast.Value
fromString =
  P.run parser >> Result.mapError P.deadEndsToString


{-| -}
parser : P.Parser Ast.Value
parser =
  P.succeed identity
    |. Yaml.Parser.Document.begins
    |. U.whitespace
    |= value
    |. Yaml.Parser.Document.ends



-- YAML / VALUE


value : P.Parser Ast.Value
value =
  P.oneOf
    [ Yaml.Parser.String.exceptions
    , recordInline
    , listInline
    , P.andThen list P.getCol
    , P.andThen (recordOrString 0) P.getCol
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
        P.oneOf
          [ P.succeed next
              |= listElement indent
          , P.succeed finish -- for lists on the same indentation level as the parent record 
          ]
    , larger = \_ -> 
        P.problem "I was looking for the next element but didn't find one."
    , ending = 
        P.succeed finish
    }


listElement : Int -> P.Parser Ast.Value
listElement indent =
  P.succeed identity 
    |. listElementBegin
    |= listElementValue indent


listElementBegin : P.Parser ()
listElementBegin = 
  P.oneOf 
    [ P.symbol "- "
    , P.symbol "-\n"
    ]


listElementValue : Int -> P.Parser Ast.Value
listElementValue indent =
  U.indented indent
    { smaller = 
        P.succeed Ast.Null_
    , exactly =
        P.succeed Ast.Null_
    , larger = \indent_ ->
        P.oneOf
          [ listInline
          , recordInline
          , list indent_
          , recordOrString indent indent_
          ]
    , ending = 
        P.succeed Ast.Null_
    }



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


recordOrString : Int -> Int -> P.Parser Ast.Value
recordOrString indent indent_ =
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
            |= if indent == 0 then U.remaining else U.multiline indent 
        ]

    property name =
      P.succeed (record indent_ name)
        |. P.chompIf U.isColon 
        |> P.andThen identity

    addRemaining string remaining =
      Ast.fromString (removeComment string ++ remaining)

    removeComment string =
      string
        |> String.split " #"
        |> List.head
        |> Maybe.withDefault ""
  in
  P.oneOf
    [ P.succeed (Ast.String_ ":")
        |. P.chompIf U.isColon
    , P.succeed identity
        |= P.oneOf [ U.singleQuotes, U.doubleQuotes ]
        |. U.spaces
        |> P.andThen withQuote
    , P.succeed identity
        |. P.chompIf (U.neither U.isColon U.isNewLine)
        |. P.chompWhile (U.neither U.isColon U.isNewLine)
        |> P.getChompedString
        |> P.andThen withString
    ]


record : Int -> String -> P.Parser Ast.Value
record indent property =
  let
    confirmed value_ =
      P.succeed (Ast.Record_ << Dict.fromList)
        |= P.loop [ ( property, value_ ) ] (recordStep indent)
  in
  recordElementValue indent
    |> P.andThen confirmed


recordStep : Int -> List Ast.Property -> P.Parser (P.Step (List Ast.Property) (List Ast.Property))
recordStep indent values =
  let finish = P.Done (List.reverse values)
      next value_ = P.Loop (value_ :: values)
  in
  U.indented indent
    { smaller = 
        P.succeed finish
    , exactly = 
        P.succeed next
          |= recordElement indent
    , larger = \_ -> 
        P.problem "I was looking for the next property but didn't find one."
    , ending = 
        P.succeed finish
    }


recordElement : Int -> P.Parser Ast.Property
recordElement indent =
  let
    property =
      P.oneOf 
        [ U.singleQuotes
        , U.doubleQuotes 
        , P.chompWhile (U.neither U.isColon U.isNewLine)
            |> P.getChompedString
        ]
  in
  P.succeed Tuple.pair 
    |= property
    |. U.spaces
    |. P.chompIf U.isColon
    |= recordElementValue indent


recordElementValue : Int -> P.Parser Ast.Value
recordElementValue indent =
  U.indented indent
    { smaller = 
        P.succeed Ast.Null_
    , exactly =
        P.oneOf
          [ list indent
          , P.succeed Ast.Null_
          ]
    , larger = \indent_ ->
        P.oneOf
          [ listInline
          , recordInline
          , list indent_
          , recordOrString indent indent_
          ]
    , ending = 
        P.succeed Ast.Null_
    }
  



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
