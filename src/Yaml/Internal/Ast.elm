module Yaml.Internal.Ast exposing (Ast, build, view)

{-|

@docs Ast, build, view

-}

import Html
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
                , map Hash <| Hash.parser (valueInline '\n') valueTopLevel
                , map Hash <| InlineHash.parser (valueInline '}')
                , map Array <| InlineArray.parser (valueInline ']')
                , map Primitive <| InlineString.parser Nothing
                ]


valueInline : Char -> Parser Ast
valueInline endChar =
    lazy <|
        \() ->
            oneOf
                [ map Hash <| InlineHash.parser (valueInline '}')
                , map Array <| InlineArray.parser (valueInline ']')
                , map Primitive <| InlineString.parser (Just endChar)
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
