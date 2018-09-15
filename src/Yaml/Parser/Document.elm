module Yaml.Parser.Document exposing (begins, ends)


import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Util as U


{-| -}
begins : P.Parser Int
begins =
  P.succeed identity
    |. P.spaces
    |= P.oneOf 
        [ P.succeed identity
            |. U.threeDashes 
            |. P.chompUntilEndOr "\n"
            |= U.nextIndent
        , P.succeed identity
            |= U.nextIndent
        ]

{-| -}
ends : P.Parser (a -> a)
ends =
  P.succeed identity
    |. P.spaces
    |. P.end
