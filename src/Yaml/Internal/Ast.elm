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
            "{ " ++ viewProperties indent properties ++ " }"

        Array elements ->
            "[ " ++ viewElements indent elements ++ " ]"


viewProperties : Int -> List ( String, Ast ) -> String
viewProperties indent =
    String.join ", " << List.map (viewProperty indent)


viewProperty : Int -> ( String, Ast ) -> String
viewProperty indent ( property, value ) =
    String.repeat indent " " ++ property ++ ": " ++ viewAst (indent + 2) value


viewElements : Int -> List Ast -> String
viewElements indent =
    String.join ", " << List.map (viewAst indent)



-- PARSER


parser : Parser Ast
parser =
    succeed identity
        |. beginning
        |= value Nothing 0
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


value : Maybe Char -> Int -> Parser Ast
value endChar indent =
    lazy <|
        \() ->
            succeed identity
                |. spacesOf indent
                |= oneOf
                    [ map Array <| Array.parser (value Nothing 0)
                    , map Hash <| CompactHash.parser <| value (Just '}') indent
                    , map Array <| CompactArray.parser <| value (Just ']') indent
                    , map Primitive <| CompactString.parser endChar
                    ]



-- GENERAL


spacesOf : Int -> Parser ()
spacesOf indent =
    ignore (Exactly indent) (\c -> c == ' ')


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


spacesOrNewLines : Parser ()
spacesOrNewLines =
    ignore zeroOrMore (\c -> c == ' ' || String.fromChar c == "\n")


ignoreUntilNewLine : Parser ()
ignoreUntilNewLine =
    ignore zeroOrMore (\c -> c /= '\n')
