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
        |= topLevelValue
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


topLevelValue : Parser Ast
topLevelValue =
    oneOf
        [ map Hash <| CompactHash.parser (value '}')
        , map Array <| CompactArray.parser (value ']')
        , map Primitive <| CompactString.parser Nothing
        ]


value : Char -> Parser Ast
value endChar =
    lazy <|
        \() ->
            oneOf
                [ map Hash <| CompactHash.parser (value '}')
                , map Array <| CompactArray.parser (value ']')
                , map Primitive <| CompactString.parser (Just endChar)
                ]



-- GENERAL


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


spacesOrNewLines : Parser ()
spacesOrNewLines =
    ignore zeroOrMore (\c -> c == ' ' || String.fromChar c == "\n")


ignoreUntilNewLine : Parser ()
ignoreUntilNewLine =
    ignore zeroOrMore (\c -> c /= '\n')
