module Yaml.Internal.Ast exposing (Ast, build)

{-|

@docs Ast, build

-}

import Char
import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
import Set


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
    threeDashes |. anythingUntilNewLine


threeDashes : Parser ()
threeDashes =
    ignore (Exactly 3) (\c -> c == '-')



-- VALUES


value : Parser Ast
value =
    lazy <|
        \() ->
            oneOf
                [ map Hash hashSingleLine
                , map Array arraySingleLine
                , map Primitive singleLineString
                ]



-- SINGLE LINE HASHES


hashSingleLine : Parser (List ( String, Ast ))
hashSingleLine =
    lazy <|
        \() ->
            succeed identity
                |. symbol "{"
                |. spaces
                |= andThen (\n -> hashSingleLineHelp [ n ]) hashSingleLineProperty
                |. spaces
                |. symbol "}"


hashSingleLineHelp : List ( String, Ast ) -> Parser (List ( String, Ast ))
hashSingleLineHelp revProperties =
    lazy <|
        \() ->
            oneOf
                [ andThen (\n -> hashSingleLineHelp (n :: revProperties)) hashPropertyNext
                , succeed (List.reverse revProperties)
                ]


hashPropertyNext : Parser ( String, Ast )
hashPropertyNext =
    lazy <|
        \() ->
            delayedCommit spaces <|
                succeed identity
                    |. symbol ","
                    |. spaces
                    |= hashSingleLineProperty


hashSingleLineProperty : Parser ( String, Ast )
hashSingleLineProperty =
    lazy <|
        \() ->
            succeed (,)
                |= fieldName
                |. spaces
                |. spacesOrSingleColon
                |. spaces
                |= value


spacesOrSingleColon : Parser ()
spacesOrSingleColon =
    oneOf [ symbol ":", spaces ]



-- SINGLE LINE ARRAY


arraySingleLine : Parser (List Ast)
arraySingleLine =
    lazy <|
        \() ->
            succeed identity
                |. symbol "["
                |. spaces
                |= andThen (\n -> arraySingleLineHelp [ n ]) value
                |. spaces
                |. symbol "]"


arraySingleLineHelp : List Ast -> Parser (List Ast)
arraySingleLineHelp revElements =
    lazy <|
        \() ->
            oneOf
                [ andThen (\n -> arraySingleLineHelp (n :: revElements)) arrayElementNext
                , succeed (List.reverse revElements)
                ]


arrayElementNext : Parser Ast
arrayElementNext =
    lazy <|
        \() ->
            delayedCommit spaces <|
                succeed identity
                    |. symbol ","
                    |. spaces
                    |= value



-- STRING


singleLineString : Parser String
singleLineString =
    lazy <|
        \() ->
            succeed identity
                |. spaces
                |= andThen (\n -> singleLineStringHelp [ n ]) singleLineStringValidateHead
                |. spaces


singleLineStringHelp : List String -> Parser String
singleLineStringHelp revStrings =
    lazy <|
        \() ->
            oneOf
                [ andThen (\n -> singleLineStringHelp (n :: revStrings)) singleLineStringNext
                , succeed (List.reverse revStrings |> String.concat)
                ]


singleLineStringNext : Parser String
singleLineStringNext =
    lazy <|
        \() ->
            delayedCommitMap (\spaces string -> spaces ++ string) keepSpaces <|
                succeed identity
                    |= singleLineStringValidateTail


singleLineStringValidateHead : Parser String
singleLineStringValidateHead =
    lazy <|
        \() ->
            keep (Exactly 1) <|
                \char ->
                    char /= '[' && char /= ']' && char /= '{' && char /= ',' && char /= '\n' && char /= ' '


singleLineStringValidateTail : Parser String
singleLineStringValidateTail =
    lazy <|
        \() ->
            keep oneOrMore <|
                \char ->
                    char /= '[' && char /= ']' && char /= '}' && char /= ',' && char /= '\n' && char /= ' '


keepSpaces : Parser String
keepSpaces =
    lazy <|
        \() ->
            keep zeroOrMore (\c -> c == ' ')



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
