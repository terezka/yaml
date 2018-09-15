module Yaml.Parser exposing (Value, toString, parser, run)

import Parser exposing (..)
import Yaml.Parser.Ast as Ast
import Yaml.Parser.Util as U
import Yaml.Parser.Document
import Yaml.Parser.String
import Yaml.Parser.Record
import Yaml.Parser.List


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
    [ Yaml.Parser.Record.inline { child = yamlValueInline }
    , Yaml.Parser.List.inline { inline = yamlValueInline, toplevel = yamlValueToplevel }
    , Yaml.Parser.List.toplevel { inline = yamlValueInline, toplevel = yamlValueToplevel } indent
    , yamlRecord True indent
    , yamlNull
    , Yaml.Parser.String.toplevel
    ]


yamlValueInline : List Char -> Parser Ast.Value
yamlValueInline endings =
  oneOf
    [ Yaml.Parser.Record.inline { child = yamlValueInline }
    , Yaml.Parser.List.inline { inline = yamlValueInline, toplevel = yamlValueToplevel }
    , Yaml.Parser.String.inline endings
    ]


yamlValueToplevelInline : Parser Ast.Value
yamlValueToplevelInline =
  lazy <| \_ -> 
    oneOf
      [ Yaml.Parser.List.inline { inline = yamlValueInline, toplevel = yamlValueToplevel }
      , Yaml.Parser.Record.inline { child = yamlValueInline }
      , yamlNull
      , Yaml.Parser.String.inline ['\n']
      ]


yamlValueToplevel : Parser Ast.Value
yamlValueToplevel =
  lazy <| \_ -> 
    oneOf
      [ Yaml.Parser.List.inline { inline = yamlValueInline, toplevel = yamlValueToplevel }
      , Yaml.Parser.Record.inline { child = yamlValueInline }
      , andThen (Yaml.Parser.List.toplevel { inline = yamlValueInline, toplevel = yamlValueToplevel }) getCol
      , andThen (yamlRecord False) getCol
      , yamlNull
      , Yaml.Parser.String.inline ['\n']
      ]


-- YAML / NULL


yamlNull : Parser Ast.Value
yamlNull =
  succeed Ast.Null_
    |. U.newLine



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
          [ map (\list -> continued ({ latest | value = list } :: rest)) (Yaml.Parser.List.toplevel { inline = yamlValueInline, toplevel = yamlValueToplevel } indent)
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
    [ Yaml.Parser.List.inline { inline = yamlValueInline, toplevel = yamlValueToplevel }
    , Yaml.Parser.Record.inline { child = yamlValueInline }
    , yamlNull
    , succeed identity 
        |= Yaml.Parser.String.inline ['\n']
    ]


yamlRecordValue : Parser Ast.Value
yamlRecordValue =
  lazy <| \_ -> 
    oneOf
      [ Yaml.Parser.List.inline { inline = yamlValueInline, toplevel = yamlValueToplevel }
      , Yaml.Parser.Record.inline { child = yamlValueInline }
      , andThen (Yaml.Parser.List.toplevel { inline = yamlValueInline, toplevel = yamlValueToplevel }) getCol
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


