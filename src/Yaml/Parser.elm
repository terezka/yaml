module Yaml.Parser exposing (Value, toString, parser, run)

import Parser exposing (..)
import Yaml.Parser.Ast as Ast
import Yaml.Parser.Util as U
import Yaml.Parser.Document
import Yaml.Parser.String
import Yaml.Parser.Record
import Yaml.Parser.List
import Yaml.Parser.Null


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
    |= andThen value Yaml.Parser.Document.begins -- TODO move get indent up to here
    |. Yaml.Parser.Document.ends



-- YAML / VALUE


value : Int -> Parser Ast.Value
value indent =
  oneOf
    [ Yaml.Parser.Record.inline { inline = valueInline }
    , Yaml.Parser.List.inline { inline = valueInline, toplevel = valueToplevel }
    , Yaml.Parser.List.toplevel { inline = valueInline, toplevel = valueToplevel } indent
    , Yaml.Parser.Record.toplevel toplevelRecordConfig True indent
    , Yaml.Parser.Null.inline
    , Yaml.Parser.String.toplevel
    ]


valueInline : List Char -> Parser Ast.Value
valueInline endings =
  lazy <| \_ -> 
    oneOf
      [ Yaml.Parser.Record.inline { inline = valueInline }
      , Yaml.Parser.List.inline { inline = valueInline, toplevel = valueToplevel }
      , Yaml.Parser.String.inline endings
      ]


valueToplevelInline : Parser Ast.Value
valueToplevelInline =
  lazy <| \_ -> 
    oneOf
      [ Yaml.Parser.List.inline { inline = valueInline, toplevel = valueToplevel }
      , Yaml.Parser.Record.inline { inline = valueInline }
      , Yaml.Parser.Null.inline
      , Yaml.Parser.String.inline ['\n']
      ]


valueToplevel : Parser Ast.Value
valueToplevel =
  lazy <| \_ -> 
    oneOf
      [ Yaml.Parser.List.inline { inline = valueInline, toplevel = valueToplevel }
      , Yaml.Parser.Record.inline { inline = valueInline }
      , andThen (Yaml.Parser.List.toplevel { inline = valueInline, toplevel = valueToplevel }) getCol
      , andThen (Yaml.Parser.Record.toplevel toplevelRecordConfig False) getCol
      , Yaml.Parser.Null.inline
      , Yaml.Parser.String.inline ['\n']
      ]


toplevelRecordConfig : Yaml.Parser.Record.Toplevel
toplevelRecordConfig =
  { inlineToplevel = valueToplevelInline
  , toplevel = valueToplevel
  , list = Yaml.Parser.List.toplevel { inline = valueInline, toplevel = valueToplevel }
  }