module Yaml.Internal.Ast exposing (Ast, build)

{-|

@docs Ast, build

-}

import Parser exposing (..)
import Yaml.Internal.Ast.Compact.Array as CompactArray
import Yaml.Internal.Ast.Compact.Hash as CompactHash
import Yaml.Internal.Ast.Compact.String as CompactString


{-| -}
type Ast
    = Primitive String
    | Hash (List ( String, Ast ))
    | Array (List Ast)


{-| -}
build : String -> Result Error Ast
build =
    run parser


parser : Parser Ast
parser =
    succeed identity
        |. beginning
        |= value
        |. spacesOrNewLines
        |. end



-- BEGINNING


beginning : Parser ()
beginning =
    oneOf
        [ documentNote |. spacesOrNewLines
        , spacesOrNewLines
        ]


documentNote : Parser ()
documentNote =
    threeDashes |. ignoreUntilNewLine


threeDashes : Parser ()
threeDashes =
    ignore (Exactly 3) (\c -> c == '-')



-- VALUES


value : Parser Ast
value =
    lazy <|
        \() ->
            oneOf
                [ map Hash (CompactHash.parser valueInner)
                , map Array (CompactArray.parser valueInner)
                , map Primitive (CompactString.parser Nothing)
                ]


valueInner : Char -> Parser Ast
valueInner endChar =
    lazy <|
        \() ->
            oneOf
                [ map Hash (CompactHash.parser valueInner)
                , map Array (CompactArray.parser valueInner)
                , map Primitive (CompactString.parser (Just endChar))
                ]



-- GENERAL


spaces : Parser ()
spaces =
    ignore zeroOrMore (\char -> char == ' ')


spacesOrNewLines : Parser ()
spacesOrNewLines =
    ignore zeroOrMore (\char -> char == ' ' || String.fromChar char == "\n")


ignoreUntilNewLine : Parser ()
ignoreUntilNewLine =
    ignore zeroOrMore (\char -> char /= '\n')
