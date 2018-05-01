module Yaml.Internal.Ast.Hash exposing (Hash, Property, parser)

{-|

@docs Hash, Property, parser

-}

import Char
import Parser exposing (..)
import Parser.LowLevel exposing (..)


{-| -}
type alias Hash value =
    List (Property value)


{-| -}
type alias Property value =
    ( String, value )


{-| -}
parser : Parser value -> Parser value -> Parser (Hash value)
parser inline value =
    succeed (\f i v -> properties inline value (i - String.length f) [ ( f, v ) ])
        |= fieldName
        |= getCol
        |. spaces
        |. symbol ":"
        |= propertyValue inline value
        |> andThen identity


propertyValue : Parser value -> Parser value -> Parser value
propertyValue inline value =
    oneOf
        [ succeed identity |= inline |. newLine
        , succeed identity |. spaces |. newLine |. spacesOrNewLines |= value
        ]


properties : Parser value -> Parser value -> Int -> Hash value -> Parser (Hash value)
properties inline value indent revProperties =
    oneOf
        [ andThen (\n -> properties inline value indent (n :: revProperties)) (nextProperty inline value indent)
        , succeed (List.reverse revProperties)
        ]


nextProperty : Parser value -> Parser value -> Int -> Parser (Property value)
nextProperty inline value indent =
    delayedCommit (spacesOf indent) <|
        succeed (,)
            |= fieldName
            |. spaces
            |. symbol ":"
            |. spaces
            |= propertyValue inline value



-- FIELD NAME


fieldName : Parser String
fieldName =
    keep oneOrMore isVarChar


isVarChar : Char -> Bool
isVarChar c =
    Char.isLower c || Char.isUpper c || Char.isDigit c || c == '_'



-- HELPERS


spacesOf : Int -> Parser ()
spacesOf indent =
    ignore (Exactly (indent - 1)) (\c -> c == ' ')


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
