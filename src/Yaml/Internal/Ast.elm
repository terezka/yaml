module Yaml.Internal.Ast exposing (Ast(..), build)

{-|

@docs Ast, build

-}

import Parser exposing (..)
import Yaml.Internal.Ast.Array as Array
import Yaml.Internal.Ast.Hash as Hash
import Yaml.Internal.Ast.Inline.Array as InlineArray
import Yaml.Internal.Ast.Inline.Hash as InlineHash
import Yaml.Internal.Ast.Inline.String as InlineString


{-| -}
type Ast
    = Primitive String
    | Hash (List ( String, Ast ))
    | Array (List Ast)


{-| -}
build : String -> Result Error Ast
build =
    run parser



-- PARSER


parser : Parser Ast
parser =
    succeed identity
        |. beginning
        |= valueTopLevel
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


valueTopLevel : Parser Ast
valueTopLevel =
    lazy <|
        \() ->
            oneOf
                [ map Array <| Array.parser (valueInline '\n') valueTopLevel
                , map Hash <| InlineHash.parser (valueInline '}')
                , map Array <| InlineArray.parser (valueInline ']')
                , andThen stringOrHash <| Hash.fieldName
                , map Primitive <| InlineString.parser Nothing
                ]


stringOrHash : String -> Parser Ast
stringOrHash s0 =
    let
        parseHash spaces =
            map Hash <| Hash.parser (valueInline '\n') valueTopLevel s0 spaces

        done s1 s2 =
            succeed (Primitive (s0 ++ s2 ++ s1))

        finish s1 parser =
            parser s1
    in
    succeed finish
        |= keepSpaces
        |= oneOf
            [ succeed parseHash |. symbol ":"
            , succeed done |= keepUntilNewLine
            ]
        |> andThen identity


valueInline : Char -> Parser Ast
valueInline endChar =
    lazy <|
        \() ->
            oneOf
                [ map Hash <| InlineHash.parser (valueInline '}')
                , map Array <| InlineArray.parser (valueInline ']')
                , andThen stringOrHash <| Hash.fieldName
                , map Primitive <| InlineString.parser (Just endChar)
                ]



-- GENERAL


keepSpaces : Parser String
keepSpaces =
    keep zeroOrMore (\c -> c == ' ')


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


keepUntilNewLine : Parser String
keepUntilNewLine =
    keep zeroOrMore (\c -> c /= '\n')


spacesOrNewLines : Parser ()
spacesOrNewLines =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')


ignoreUntilNewLine : Parser ()
ignoreUntilNewLine =
    ignore zeroOrMore (\c -> c /= '\n')
