module Yaml.Parser exposing (Value, toString, parser, run)

import Parser as P exposing ((|=), (|.))
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
run : String -> Result (List P.DeadEnd) Ast.Value
run =
  P.run parser


{-| -}
parser : P.Parser Ast.Value
parser =
  P.succeed identity
    |= P.andThen value Yaml.Parser.Document.begins -- TODO move get indent up to here
    |. Yaml.Parser.Document.ends



-- YAML / VALUE


value : Int -> P.Parser Ast.Value
value indent =
  P.oneOf
    [ Yaml.Parser.Record.inline { inline = valueInline }
    , Yaml.Parser.List.inline { child = valueInline }
    , Yaml.Parser.List.toplevel { child = valueToplevel } indent
    , Yaml.Parser.Record.toplevel toplevelRecordConfig True indent
    , Yaml.Parser.Null.inline
    , Yaml.Parser.String.toplevel
    ]


valueInline : List Char -> P.Parser Ast.Value
valueInline endings =
  P.lazy <| \_ -> 
    P.oneOf
      [ Yaml.Parser.Record.inline { inline = valueInline }
      , Yaml.Parser.List.inline { child = valueInline }
      , Yaml.Parser.String.inline endings
      ]


valueToplevelInline : P.Parser Ast.Value
valueToplevelInline =
  P.lazy <| \_ -> 
    P.oneOf
      [ Yaml.Parser.List.inline { child = valueInline }
      , Yaml.Parser.Record.inline { inline = valueInline }
      , Yaml.Parser.Null.inline
      , Yaml.Parser.String.inline ['\n']
      ]


valueToplevel : P.Parser Ast.Value
valueToplevel =
  P.lazy <| \_ -> 
    P.oneOf
      [ Yaml.Parser.List.inline { child = valueInline }
      , Yaml.Parser.Record.inline { inline = valueInline }
      , P.andThen (Yaml.Parser.List.toplevel { child = valueToplevel }) P.getCol
      , P.andThen (Yaml.Parser.Record.toplevel toplevelRecordConfig False) P.getCol
      , Yaml.Parser.Null.inline
      , Yaml.Parser.String.inline ['\n']
      ]



-- CONFIGS


toplevelRecordConfig : Yaml.Parser.Record.Toplevel
toplevelRecordConfig =
  { childInline = valueToplevelInline
  , childToplevel = valueToplevel
  , list = Yaml.Parser.List.toplevel { child = valueToplevel }
  }