module Yaml.Internal.Ast exposing (Ast, build, view)

{-|

@docs Ast, build, view

-}

import Html
import Parser exposing (..)
import Yaml.Internal.Ast.Array as Array
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



-- PRINT


{-| -}
view : Result Error Ast -> Html.Html msg
view result =
    Html.code [] <|
        case result of
            Ok ast ->
                [ Html.text (viewAst 0 ast) ]

            Err error ->
                [ Html.text (toString error) ]


viewAst : Int -> Ast -> String
viewAst indent ast =
    case ast of
        Primitive text ->
            String.repeat indent " " ++ text

        Hash properties ->
            "{ " ++ (List.map (viewProperty indent) properties |> String.join ", ") ++ " }"

        Array elements ->
            "[ " ++ (List.map (viewAst indent) elements |> String.join ", ") ++ " ]"


viewProperty : Int -> ( String, Ast ) -> String
viewProperty indent ( property, value ) =
    String.repeat indent " " ++ property ++ ": " ++ viewAst (indent + 2) value



-- PARSER


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
                [ map Array <| Array.parser value
                , map Hash <| CompactHash.parser (valueInside '}')
                , map Array <| CompactArray.parser (valueInside ']')
                , map Primitive <| CompactString.parser Nothing
                ]


valueInside : Char -> Parser Ast
valueInside endChar =
    lazy <|
        \() ->
            oneOf
                [ map Array <| Array.parser value
                , map Hash <| CompactHash.parser (valueInside '}')
                , map Array <| CompactArray.parser (valueInside ']')
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
