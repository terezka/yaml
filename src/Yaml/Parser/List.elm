module Yaml.Parser.List exposing (inline)


import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Util as U
import Yaml.Parser.Ast as Ast


type alias Config =
  { child : List Char -> P.Parser Ast.Value }


inline : Config -> P.Parser Ast.Value
inline config =
  P.succeed Ast.List_
    |. P.symbol "["
    |. U.spaces
    |= P.oneOf
        [ P.succeed []
            |. P.symbol "}"
        , P.loop [] (inlineEach config)
        ]


inlineEach : Config -> List Ast.Value -> P.Parser (P.Step (List Ast.Value) (List Ast.Value))
inlineEach config values =
  P.succeed (\v next -> next (v :: values))
    |= config.child [',', ']']
    |. U.spaces
    |= P.oneOf
        [ P.succeed P.Loop |. P.symbol "," 
        , P.succeed (P.Done << List.reverse) |. P.symbol "]"
        ]
    |. U.spaces


