module Yaml.Parser.String exposing (toplevel, inline, exceptions)


import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Util as U
import Yaml.Parser.Ast as Ast


{-| -}
toplevel : P.Parser Ast.Value
toplevel =
  P.oneOf
    [ P.succeed Ast.String_
        |= U.singleQuotes
    , P.succeed Ast.String_
        |= U.doubleQuotes
    , P.succeed Ast.fromString
        |= U.remaining
    ]        


{-| -}
inline : List Char -> P.Parser Ast.Value
inline endings =
   P.oneOf
    [ P.succeed Ast.String_
        |= U.singleQuotes
    , P.succeed Ast.String_
        |= U.doubleQuotes
    , P.succeed Ast.fromString
        |= U.characters endings
    ]


{-| -}
exceptions : P.Parser Ast.Value
exceptions =
    let dashed s = "---" ++ s in
    P.succeed (Ast.String_ << dashed)
        |. U.threeDashes
        |= U.remaining

