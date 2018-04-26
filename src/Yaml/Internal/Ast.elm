module Yaml.Internal.Ast exposing (Ast, build)

{-|

@docs Ast, build

-}

import Char
import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
import Parser.LowLevel as Parser exposing (..)
import Set


{-| -}
type Ast
    = Primitive String
    | Record (List ( String, Ast ))
    | Array Ast


{-| -}
build : String -> Result Error Ast
build =
    run parser


parser : Parser Ast
parser =
    succeed identity
        |. beginning
        |= (fieldName |> map Primitive)



-- BEGINNING


beginning : Parser ()
beginning =
    oneOf
        [ documentNote |. spacesOrNewLines
        , spacesOrNewLines
        ]


documentNote : Parser ()
documentNote =
    threeDashes |. anythingUntilNewLine


threeDashes : Parser ()
threeDashes =
    ignore (Exactly 3) (\c -> c == '-')



-- VALUES


value : Parser Ast
value =
    oneOf
        [ record |> map Record
        , fieldName |> map Primitive
        ]



-- HASHES


record : Parser (List ( String, Ast ))
record =
    Parser.record spaces field


field : Parser ( String, Ast )
field =
    Parser.succeed (,)
        |= fieldName
        |. spaces
        |. Parser.symbol ":"
        |. spaces
        |= (fieldName |> map Primitive)



-- FIELD NAME


fieldName : Parser String
fieldName =
    variable (always True) isVarChar keywords


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || (char == '_')


keywords : Set.Set String
keywords =
    Set.empty



-- GENERAL


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


spacesOrNewLines : Parser ()
spacesOrNewLines =
    ignore zeroOrMore (\c -> c == ' ' || String.fromChar c == "\n")


anythingUntilNewLine : Parser ()
anythingUntilNewLine =
    ignore zeroOrMore (\c -> c /= '\n')


whitespace : Parser ()
whitespace =
    Parser.whitespace
        { allowTabs = True
        , lineComment = LineComment "#"
        , multiComment = NoMultiComment
        }
