module Yaml.Internal.Ast.Inline.Array exposing (Array, parser)

{-|

@docs Array, parser

-}

import Parser exposing (..)


{-| -}
type alias Array value =
    List value


{-| -}
parser : Parser value -> Parser (Array value)
parser value =
    succeed identity
        |. symbol "["
        |. spaces
        |= firstElement value
        |. spaces
        |. symbol "]"


firstElement : Parser value -> Parser (Array value)
firstElement value =
    oneOf
        [ andThen (\n -> elements value [ n ]) value
        , succeed []
        ]


elements : Parser value -> Array value -> Parser (Array value)
elements value revElements =
    oneOf
        [ andThen (\n -> elements value (n :: revElements)) (nextElement value)
        , succeed (List.reverse revElements)
        ]


nextElement : Parser value -> Parser value
nextElement value =
    delayedCommit spaces <|
        succeed identity
            |. symbol ","
            |. spaces
            |= value



-- HELPERS


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')
