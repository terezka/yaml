module Yaml.Parser.List exposing (Toplevel, toplevel, Inline, inline)


import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Util as U
import Yaml.Parser.Ast as Ast


-- TOP LEVEL


type alias Toplevel =
  { child : P.Parser Ast.Value }


{-| -}
toplevel : Toplevel -> Int -> P.Parser Ast.Value
toplevel config indent =
  let
    withValue value =
      P.succeed Ast.List_
        |= P.loop [ value ] (toplevelEach config indent)
  in
  toplevelNewEntry config
    |> P.andThen withValue


toplevelEach : Toplevel -> Int -> List Ast.Value -> P.Parser (P.Step (List Ast.Value) (List Ast.Value))
toplevelEach config indent values =
  let finish = P.Done (List.reverse values)
      next value = P.Loop (value :: values)
      continued = P.Loop
  in
  U.checkIndent indent
    { smaller = P.succeed finish
    , exactly = P.oneOf [ P.map next (toplevelNewEntry config), P.succeed finish ]
    , larger  = P.map continued << toplevelContinuedEntry config values
    , ending = P.succeed finish
    }


toplevelNewEntry : Toplevel -> P.Parser Ast.Value
toplevelNewEntry config =
  P.succeed identity
    |. U.dash
    |= P.oneOf 
        [ P.succeed Ast.Null_
            |. U.newLine
        , P.succeed identity
            |. U.space
            |. U.spaces
            |= config.child
        ]


toplevelContinuedEntry : Toplevel -> List Ast.Value -> Int -> P.Parser (List Ast.Value)
toplevelContinuedEntry config values subIndent =
  let
    coalesce value =
      case ( values, value ) of
        ( Ast.Null_ :: rest, _ ) -> 
          P.succeed (value :: rest)

        ( Ast.String_ prev :: rest, Ast.String_ new ) -> 
          P.succeed (Ast.String_ (prev ++ " " ++ new) :: rest)

        ( _ :: rest, Ast.String_ _ ) -> -- TODO don't skip new lines
          P.problem "I was parsing a record, but I got more strings when expected a new property!"

        ( _, _ ) -> 
          P.succeed (value :: values)
  in
  P.andThen coalesce config.child



-- INLINE


type alias Inline =
  { child : List Char -> P.Parser Ast.Value }


inline : Inline -> P.Parser Ast.Value
inline config =
  P.succeed Ast.List_
    |. P.symbol "["
    |. U.spaces
    |= P.oneOf
        [ P.succeed []
            |. P.symbol "}"
        , P.loop [] (inlineEach config)
        ]


inlineEach : Inline -> List Ast.Value -> P.Parser (P.Step (List Ast.Value) (List Ast.Value))
inlineEach config values =
  P.succeed (\v next -> next (v :: values))
    |= config.child [',', ']']
    |. U.spaces
    |= P.oneOf
        [ P.succeed P.Loop |. P.symbol "," 
        , P.succeed (P.Done << List.reverse) |. P.symbol "]"
        ]
    |. U.spaces


