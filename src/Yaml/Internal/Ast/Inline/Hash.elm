module Yaml.Internal.Ast.Inline.Hash exposing (Hash, Property, parser)

{-|

@docs Hash, Property, parser

-}

import Char
import Parser exposing (..)


{-| -}
type alias Hash value =
    List (Property value)


{-| -}
type alias Property value =
    ( String, value )


{-| -}
parser : Parser value -> Parser (Hash value)
parser value =
    succeed identity
        |. symbol "{"
        |. spaces
        |= firstProperty value
        |. spaces
        |. symbol "}"


firstProperty : Parser value -> Parser (Hash value)
firstProperty value =
    oneOf
        [ andThen (\n -> properties value [ n ]) (property value)
        , succeed []
        ]


properties : Parser value -> List (Property value) -> Parser (Hash value)
properties value revProperties =
    oneOf
        [ andThen (\n -> properties value (n :: revProperties)) (nextProperty value)
        , succeed (List.reverse revProperties)
        ]


nextProperty : Parser value -> Parser (Property value)
nextProperty value =
    delayedCommit spaces <|
        succeed identity
            |. symbol ","
            |. spaces
            |= property value


property : Parser value -> Parser (Property value)
property value =
    succeed (,)
        |= fieldName
        |. spaces
        |. oneOf [ symbol ":", spaces ]
        |. spaces
        |= value



-- FIELD NAME


fieldName : Parser String
fieldName =
    keep oneOrMore isVarChar


isVarChar : Char -> Bool
isVarChar c =
    Char.isLower c || Char.isUpper c || Char.isDigit c || c == '_' || c == '/' || c == '.'



-- HELPERS


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')
