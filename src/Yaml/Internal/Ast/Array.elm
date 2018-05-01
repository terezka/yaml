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
    element value
        |> andThen
            (\v ->
                getCol
                    |> andThen (\i -> withIndentLevel i (elements value [ v ]))
            )


elements : Parser value -> Array value -> Parser (Array value)
elements value revElements =
    oneOf
        [ andThen (\n -> elements value (n :: revElements)) (nextElement value)
        , succeed (List.reverse revElements)
        ]


nextElement : Parser value -> Parser value
nextElement value =
    delayedCommit spaces (element value)


element : Parser value -> Parser value
element value =
    succeed identity
        |. elementBeginning
        |. spaces
        |= value
        |. ignoreUntilNewLine


elementBeginning : Parser ()
elementBeginning =
    symbol "-" |. ignore (Exactly 1) (\c -> c == ' ')



-- HELPERS


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


ignoreUntilNewLine : Parser ()
ignoreUntilNewLine =
    spaces |. ignore (Exactly 1) (\c -> c == '\n')
