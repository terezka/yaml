module Yaml.Internal.Ast.Array exposing (Array, parser)

{-|

@docs Array, parser

-}

import Parser exposing (..)
import Parser.LowLevel exposing (..)


{-| -}
type alias Array value =
    List value


{-| -}
parser : Parser value -> Parser (Array value)
parser value =
    succeed (\i v -> elements value i [ v ])
        |. symbol "-"
        |= getCol
        |= element value
        |> andThen identity


elements : Parser value -> Int -> Array value -> Parser (Array value)
elements value indent revElements =
    oneOf
        [ andThen (\n -> elements value indent (n :: revElements)) (nextElement value indent)
        , succeed (List.reverse revElements)
        ]


nextElement : Parser value -> Int -> Parser value
nextElement value indent =
    delayedCommit (spacesOf indent) <|
        succeed identity
            |. symbol "-"
            |= element value


element : Parser value -> Parser value
element value =
    oneOf
        [ succeed identity |. oneSpace |= value |. newLine
        , succeed identity |. spaces |. newLine |. spacesOrNewLines |= value
        ]



-- HELPERS


spacesOf : Int -> Parser ()
spacesOf indent =
    ignore (Exactly (indent - 2)) (\c -> c == ' ')


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


oneSpace : Parser ()
oneSpace =
    ignore (Exactly 1) (\c -> c == ' ')


newLine : Parser ()
newLine =
    ignore (Exactly 1) (\c -> c == '\n')


spacesOrNewLines : Parser ()
spacesOrNewLines =
    ignore zeroOrMore (\c -> c == ' ' || String.fromChar c == "\n")
