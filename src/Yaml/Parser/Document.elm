module Yaml.Parser.Document exposing (begins, ends)


import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Util as U


{-| -}
begins : P.Parser (a -> a)
begins =
  P.oneOf
    [ P.succeed identity
        |. U.whitespace
        |. P.andThen dashes U.nextIndent
    , P.succeed identity
        |. U.whitespace
    ]


dashes : Int -> P.Parser (a -> a)
dashes indent =
  if indent == 1 then
    P.oneOf
      [ P.succeed identity
          |. U.threeDashes
          |. P.oneOf [ U.space, U.newLine ]
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
    |. P.oneOf [ U.threeDots, U.whitespace ]
    |. P.end
