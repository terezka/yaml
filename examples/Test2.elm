module Test2 exposing (..)

import Html
import Parser exposing (..)


main : Html.Html msg
main =
    Html.code [] [ Html.text (toString test) ]


test : Result Parser.Error String
test =
    run parser "[ 1 2 3 ]"


parser : Parser String
parser =
    succeed String.concat
        |. symbol "["
        |. ignoreSpaces
        |= andThen (\n -> intListHelp [ toString n ]) int
        |. ignoreSpaces
        |. symbol "]"


intListHelp : List String -> Parser (List String)
intListHelp revInts =
    oneOf
        [ nextInt |> andThen (\n -> intListHelp (n :: revInts))
        , succeed (List.reverse revInts)
        ]


nextInt : Parser String
nextInt =
    delayedCommitMap (\spaces string -> spaces ++ string) keepSpaces <|
        succeed toString
            |= int


keepSpaces : Parser String
keepSpaces =
    keep zeroOrMore (\c -> c == ' ')


ignoreSpaces : Parser ()
ignoreSpaces =
    ignore zeroOrMore (\c -> c == ' ')
