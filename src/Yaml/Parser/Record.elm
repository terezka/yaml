module Yaml.Parser.Record exposing (inline)


import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Util as U
import Yaml.Parser.Ast as Ast


type alias Config =
  { child : List Char -> P.Parser Ast.Value }


{-| -}
inline : Config -> P.Parser Ast.Value
inline config =
  P.succeed Ast.Record_
    |. P.symbol "{"
    |. U.spaces
    |= P.oneOf
        [ P.succeed [] |. P.symbol "}"
        , P.loop [] (inlineEach config)
        ]


inlineEach : Config -> List Ast.Property -> P.Parser (P.Step (List Ast.Property) (List Ast.Property))
inlineEach config properties =
  let
    withProperty name =
      case name of 
        Ok validName ->
          P.succeed (\v next -> next (Ast.Property validName v :: properties))
            |= inlineValue config
            |= P.oneOf
                [ P.succeed P.Loop |. U.comma
                , P.succeed (P.Done << List.reverse) |. P.symbol "}"
                ]
            |. U.spaces

        Err _ -> 
          errorMissingColon
  in
  U.propertyName False 
    |> P.andThen withProperty


inlineValue : Config -> P.Parser Ast.Value
inlineValue config =
  P.oneOf
    [ P.succeed identity
        |. P.oneOf [ U.space, U.newLine ]
        |. U.whitespace -- TODO ?
        |= config.child [',', '}']
        |. U.spaces
    , P.succeed ()
        |. P.chompIf (\c -> c /= ',' && c /= '}' && c /= '\n')
        |> P.andThen (\_ -> errorMissingSpaceAfterColon)
    , P.succeed Ast.Null_
    ]


errorMissingColon : P.Parser a 
errorMissingColon =
  P.problem "I was parsing an inline record, but I couldn't find the \":\"!"


errorMissingSpaceAfterColon : P.Parser a 
errorMissingSpaceAfterColon =
  P.problem "I was parsing an inline record, but missing a space between the \":\" and the value!"

