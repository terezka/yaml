module Yaml.Parser.String exposing (toplevel, inline)


import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Util as U
import Yaml.Parser.Ast as Ast


{-| -}
toplevel : P.Parser Ast.Value
toplevel =
  let
    multiline result =
      P.oneOf
        [ P.map (\i -> P.Loop (i :: result)) U.lineOfCharacters
        , P.succeed (P.Done (List.reverse result |> String.concat))
        ]
  in
  P.oneOf
    [ P.succeed Ast.String_
        |= U.singleQuotes
    , P.succeed Ast.String_
        |= U.doubleQuotes
    , P.succeed Ast.fromString
        |= P.loop [] multiline
    ]        


{-| -}
inline : List Char -> P.Parser Ast.Value
inline endings =
   P.oneOf
    [ P.succeed Ast.String_
        |= U.singleQuotes
        |. U.anyOf endings
    , P.succeed Ast.String_
        |= U.doubleQuotes
        |. U.anyOf endings
    , P.succeed Ast.fromString
        |= U.characters endings
    ]
