module Yaml.Parser.Util exposing 
  ( colon, comma, dash, threeDashes, space, spaces, newLine, newLines, whitespace
  , anyOf
  , singleQuotes, doubleQuotes, lineOfCharacters, characters
  , nextIndent, checkIndent
  , propertyName
  )

import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Ast as Ast


{-| -}
colon : P.Parser ()
colon =
  P.symbol ":"


{-| -}
comma : P.Parser ()
comma =
  P.symbol ","


{-| -}
dash : P.Parser ()
dash =
  P.symbol "-"


{-| -}
threeDashes : P.Parser ()
threeDashes =
  P.symbol "---"


{-| -}
space : P.Parser ()
space =
  P.symbol " "


{-| -}
spaces : P.Parser ()
spaces =
  P.chompWhile (\c -> c == ' ')


{-| -}
whitespace : P.Parser ()
whitespace =
  P.spaces


{-| -}
newLines : P.Parser ()
newLines =
  P.chompWhile (\c -> c == '\n')


{-| -}
newLine : P.Parser ()
newLine =
  P.chompIf (\c -> c == '\n')



-- OTHER


{-| -}
anyOf : List Char -> P.Parser ()
anyOf endings =
  P.chompIf (\c -> List.member c endings)



-- STRINGS


{-| -}
characters : List Char -> P.Parser String
characters endings =
  P.succeed ()
    |. P.chompWhile (\c -> not (List.member c endings))
    |> P.getChompedString


{-| -}
singleQuotes : P.Parser String
singleQuotes =
  P.succeed (String.replace "\\" "\\\\")
    |. P.symbol "'"
    |= characters ['\'']
    |. P.symbol "'"
    |. spaces


{-| -}
doubleQuotes : P.Parser String
doubleQuotes =
  P.succeed identity
    |. P.symbol "\""
    |= characters ['"']
    |. P.symbol "\""
    |. spaces


{-| -}
lineOfCharacters : P.Parser String
lineOfCharacters =
  P.succeed ()
    |. P.chompIf (\c -> c /= '\n')
    |. P.chompUntilEndOr "\n"
    |> P.getChompedString



-- INDENT


{-| -}
nextIndent : P.Parser Int
nextIndent =
  P.loop 0 nextIndentHelp


nextIndentHelp : Int -> P.Parser (P.Step Int Int)
nextIndentHelp _ =
  P.succeed (\i next -> next i)
    |. spaces
    |= P.getCol
    |= P.oneOf 
        [ P.succeed P.Loop |. newLine
        , P.succeed P.Done
        ]


{-| -}
checkIndent : Int -> { smaller : P.Parser a, exactly : P.Parser a, larger : Int -> P.Parser a, ending : P.Parser a } -> P.Parser a
checkIndent indent next =
  let check actual =
        P.oneOf
          [ P.andThen (always next.ending) P.end 
          , if actual == indent then next.exactly
            else if actual > indent then next.larger actual
            else next.smaller
          ]
        
  in
  P.andThen check nextIndent



-- PROPERTY NAME


{-| -}
propertyName : Bool -> P.Parser (Result Ast.Value String)
propertyName first =
  let remaining =
        if first then everything else characters ['\n']

      everything =
        P.succeed ()
          |. P.chompWhile (always True)
          |> P.getChompedString

      valid = Ok
      invalid s2 s1 = 
        Err (Ast.fromString (s1 ++ s2))
  in
  P.succeed apply
    |= P.oneOf
        [ P.succeed identity
            |= singleQuotes
        , P.succeed identity
            |= doubleQuotes
        , P.succeed String.trim
            |= characters [':', '\n']
        ]
    |= P.oneOf
        [ P.succeed valid
            |. colon
        , P.succeed invalid
            |= remaining
        ]

apply : a -> (a -> b) -> b
apply v f =
  f v