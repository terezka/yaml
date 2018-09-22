module Yaml.Parser.Document exposing (begins, ends)


import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Util as U


{-| -}
begins : P.Parser (a -> a)
begins =
  P.oneOf
    [ P.succeed identity
        |. U.whitespace
        |. P.andThen dashes P.getCol
    , P.succeed identity
        |. U.whitespace
    ]


dashes : Int -> P.Parser (a -> a)
dashes indent =
  if indent == 1 then
    P.oneOf
      [ P.succeed identity
          |. U.threeDashes
          |. U.whitespace
      , P.succeed identity
          |. U.whitespace
      ]
  else
    P.succeed identity
      |. U.whitespace


{-| -}
ends : P.Parser (a -> a)
ends =
  P.succeed identity
    |. U.whitespace
    |. P.oneOf [ U.threeDots, P.succeed () ]
    |. U.whitespace
    |. P.end

