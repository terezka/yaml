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
    |= andThen yamlValue Yaml.Parser.Document.begins -- TODO move get indent up to here
    |. Yaml.Parser.Document.ends



-- YAML / VALUE


yamlValue : Int -> Parser Ast.Value
yamlValue indent =
  oneOf
    [ Yaml.Parser.Record.inline { inline = yamlValueInline }
    , Yaml.Parser.List.inline { inline = yamlValueInline, toplevel = yamlValueToplevel }
    , Yaml.Parser.List.toplevel { inline = yamlValueInline, toplevel = yamlValueToplevel } indent
    , Yaml.Parser.Record.toplevel
        { inlineToplevel = yamlValueToplevelInline
        , toplevel = yamlValueToplevel
        , list = Yaml.Parser.List.toplevel { inline = yamlValueInline, toplevel = yamlValueToplevel }
        } True indent
    , Yaml.Parser.Null.inline
    , Yaml.Parser.String.toplevel
    ]


yamlValueInline : List Char -> Parser Ast.Value
yamlValueInline endings =
  lazy <| \_ -> 
    oneOf
      [ Yaml.Parser.Record.inline { inline = yamlValueInline }
      , Yaml.Parser.List.inline { inline = yamlValueInline, toplevel = yamlValueToplevel }
      , Yaml.Parser.String.inline endings
      ]


yamlValueToplevelInline : Parser Ast.Value
yamlValueToplevelInline =
  lazy <| \_ -> 
    oneOf
      [ Yaml.Parser.List.inline { inline = yamlValueInline, toplevel = yamlValueToplevel }
      , Yaml.Parser.Record.inline { inline = yamlValueInline }
      , Yaml.Parser.Null.inline
      , Yaml.Parser.String.inline ['\n']
      ]


yamlValueToplevel : Parser Ast.Value
yamlValueToplevel =
  lazy <| \_ -> 
    oneOf
      [ Yaml.Parser.List.inline { inline = yamlValueInline, toplevel = yamlValueToplevel }
      , Yaml.Parser.Record.inline { inline = yamlValueInline }
      , andThen (Yaml.Parser.List.toplevel { inline = yamlValueInline, toplevel = yamlValueToplevel }) getCol
      , andThen (Yaml.Parser.Record.toplevel
                  { inlineToplevel = yamlValueToplevelInline
                  , toplevel = yamlValueToplevel
                  , list = Yaml.Parser.List.toplevel { inline = yamlValueInline, toplevel = yamlValueToplevel }
                  } False) getCol
      , Yaml.Parser.Null.inline
      , Yaml.Parser.String.inline ['\n']
      ]

