module Test exposing (..)

import Html
import Parser exposing (..)


main : Html.Html msg
main =
    Html.code [] [ Html.text (toString test) ]


test : Result Parser.Error String
test =
    run parser "nice cow }"


parser : Parser String
parser =
    succeed identity
        |. spacing
        |= andThen (\w -> stringHelp [ w ]) stringValid
        |. spacing


stringHelp : List String -> Parser String
stringHelp revStrings =
    oneOf
        [ andThen (\w -> stringHelp (w :: revStrings)) stringValidNext
        , succeed (List.reverse revStrings |> String.concat)
        ]


stringValidNext : Parser String
stringValidNext =
    delayedCommit spaces <|
        succeed identity
            |= stringValid


stringValid : Parser String
stringValid =
    keep oneOrMore (\c -> c /= ' ' && c /= '}' && c /= ']')


spaces : Parser String
spaces =
    keep zeroOrMore (\c -> c == ' ')


spacing : Parser ()
spacing =
    ignore zeroOrMore (\c -> c == ' ')
