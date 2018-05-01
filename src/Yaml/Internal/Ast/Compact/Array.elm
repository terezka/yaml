module Yaml.Internal.Ast.Compact.Array exposing (Array, parser)

{-|

@docs Array, parser

-}

import Parser exposing (..)


{-| -}
type alias Array value =
    List value


{-| -}
parser : (Char -> Parser value) -> Parser (Array value)
parser value =
    succeed identity
        |. symbol "["
        |. spaces
        |= andThen (\n -> elements value [ n ]) (value ']')
        |. spaces
        |. symbol "]"


elements : (Char -> Parser value) -> Array value -> Parser (Array value)
elements value revElements =
    oneOf
        [ andThen (\n -> elements value (n :: revElements)) (nextElement value)
        , succeed (List.reverse revElements)
        ]


nextElement : (Char -> Parser value) -> Parser value
nextElement value =
    delayedCommit spaces <|
        succeed identity
            |. symbol ","
            |. spaces
            |= value ']'



-- HELPERS


spaces : Parser ()
spaces =
    ignore zeroOrMore (\char -> char == ' ')
