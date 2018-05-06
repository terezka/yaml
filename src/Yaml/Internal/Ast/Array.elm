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
parser : Parser value -> Parser value -> Parser (Array value)
parser inline value =
    succeed (\i v -> elements inline value i [ Debug.log "v" <| v ])
        |. symbol "-"
        |= getCol
        |= element inline value
        |> andThen identity


element : Parser value -> Parser value -> Parser value
element inline value =
    oneOf
        [ map (Debug.log "in") <| succeed identity |. oneSpace |= inline |. newLine
        , succeed identity |. spaces |. newLine |. spacesOrNewLines |= value
        ]


elements : Parser value -> Parser value -> Int -> Array value -> Parser (Array value)
elements inline value indent revElements =
    oneOf
        [ andThen (\n -> elements inline value indent (n :: revElements)) (nextElement inline value indent)
        , succeed (List.reverse revElements)
        ]


nextElement : Parser value -> Parser value -> Int -> Parser value
nextElement inline value indent =
    delayedCommit (spacesOf indent) <|
        succeed (Debug.log "v")
            |. symbol "-"
            |= element inline value



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
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')
