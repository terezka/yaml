module Yaml.Internal.Ast.Compact.String exposing (parser)

{-|

@docs parser

-}

import Parser exposing (..)


parser : Maybe Char -> Parser String
parser maybeEndChar =
    succeed identity
        |. spaces
        |= andThen (\n -> strings maybeEndChar [ n ]) stringHead
        |. spaces


strings : Maybe Char -> List String -> Parser String
strings maybeEndChar revStrings =
    oneOf
        [ andThen (\n -> strings maybeEndChar (n :: revStrings)) (nextString maybeEndChar)
        , succeed (List.reverse revStrings |> String.concat)
        ]


nextString : Maybe Char -> Parser String
nextString maybeEndChar =
    delayedCommitMap (\spaces string -> spaces ++ string) keepSpaces <|
        succeed identity
            |= stringTail maybeEndChar


stringHead : Parser String
stringHead =
    keep (Exactly 1) <|
        \char ->
            char /= ',' && char /= '\n' && char /= ' '


stringTail : Maybe Char -> Parser String
stringTail maybeEndChar =
    let
        endCondition char =
            case maybeEndChar of
                Just endChar ->
                    char /= endChar

                Nothing ->
                    False
    in
    keep oneOrMore <|
        \char ->
            endCondition char && char /= ',' && char /= '\n' && char /= ' '



-- HELPERS


spaces : Parser ()
spaces =
    ignore zeroOrMore (\char -> char == ' ')


keepSpaces : Parser String
keepSpaces =
    lazy <|
        \() ->
            keep zeroOrMore (\c -> c == ' ')
