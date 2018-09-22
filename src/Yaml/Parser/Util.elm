module Yaml.Parser.Util exposing 
  ( isColon, isComma, isDot, isDash, isHash, isSpace, isNewLine, isListStart, isListEnd, isRecordStart, isRecordEnd, either, neither, neither3
  , colon, comma, dash, threeDashes, threeDots, space, spaces, newLine, newLines, whitespace, multiline
  , singleQuotes, doubleQuotes, remaining
  , indented
  )

import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Ast as Ast



-- QUESTIONS


{-| -}
isColon : Char -> Bool
isColon =
  is ':'


{-| -}
isComma : Char -> Bool
isComma =
  is ','


{-| -}
isDot : Char -> Bool
isDot =
  is '.'


{-| -}
isDash : Char -> Bool
isDash =
  is '-'


{-| -}
isHash : Char -> Bool
isHash =
  is '#'


{-| -}
isSpace : Char -> Bool
isSpace =
  is ' '


{-| -}
isNewLine : Char -> Bool
isNewLine =
  is '\n'


{-| -}
isListStart : Char -> Bool
isListStart =
  is '['


{-| -}
isListEnd : Char -> Bool
isListEnd =
  is ']'


{-| -}
isRecordStart : Char -> Bool
isRecordStart =
  is '{'


{-| -}
isRecordEnd : Char -> Bool
isRecordEnd =
  is '}'


{-| -}
isSingleQuote : Char -> Bool
isSingleQuote =
  is '\''


{-| -}
isDoubleQuote : Char -> Bool
isDoubleQuote =
  is '"'


{-| -}
either : (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
either f1 f2 char =
  f1 char || f2 char


{-| -}
neither : (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
neither f1 f2 char =
  not (f1 char) && not (f2 char)


{-| -}
neither3 : (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
neither3 f1 f2 f3 char =
  not (f1 char) && not (f2 char) && not (f3 char)


{-| -}
is : Char -> Char -> Bool
is searched char =
  char == searched



-- 


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
hash : P.Parser ()
hash =
  P.symbol "#"


{-| -}
threeDots : P.Parser ()
threeDots =
  P.symbol "..."


{-| -}
space : P.Parser ()
space =
  P.symbol " "


{-| -}
spaces : P.Parser ()
spaces =
  let actualSpaces = 
        P.chompWhile (\c -> c == ' ')
  in
  P.succeed ()
    |. actualSpaces
    |. P.oneOf [ comment, P.succeed () ]
    |. actualSpaces
  

{-| -}
whitespace : P.Parser ()
whitespace =
  P.succeed ()
    |. P.spaces
    |. P.oneOf [ comment, P.succeed () ]
    |. P.spaces


{-| -}
newLines : P.Parser ()
newLines =
  let actualNewLines = 
        P.chompWhile (\c -> c == '\n')
  in
  P.succeed ()
    |. actualNewLines
    |. P.oneOf [ comment, P.succeed () ]
    |. actualNewLines
  

{-| -}
newLine : P.Parser ()
newLine =
  P.chompIf (\c -> c == '\n')


{-| -}
comment : P.Parser ()
comment =
  P.succeed ()
    |. hash
    |. P.chompUntilEndOr "\n"



-- STRINGS


{-| -}
multiline : Int -> P.Parser String
multiline indent =
  P.loop [] (multilineStep indent)


multilineStep : Int -> List String -> P.Parser (P.Step (List String) String)
multilineStep indent lines =
  let
    conclusion line indent_ =
      if indent_ > indent then
        P.Loop (line :: lines)
      else
        P.Done (String.join "\n" (List.reverse (line :: lines)))
  in
  P.succeed conclusion
    |= (P.chompWhile (not << isNewLine) |> P.getChompedString)
    |. P.chompIf isNewLine
    |. P.spaces
    |= P.getCol


{-| -}
characters : (Char -> Bool) -> P.Parser String
characters isOk =
  P.succeed ()
    |. P.chompWhile isOk
    |> P.getChompedString


{-| -}
singleQuotes : P.Parser String
singleQuotes =
  P.succeed (String.replace "\\" "\\\\")
    |. P.symbol "'"
    |= characters (not << isSingleQuote)
    |. P.symbol "'"
    |. spaces


{-| -}
doubleQuotes : P.Parser String
doubleQuotes =
  P.succeed identity
    |. P.symbol "\""
    |= characters (not << isDoubleQuote)
    |. P.symbol "\""
    |. spaces


{-| -}
remaining : P.Parser String
remaining =
  P.succeed ()
    |. P.chompUntilEndOr "\n...\n"
    |> P.getChompedString



-- INDENT


{-| -}
indented : Int -> { smaller : P.Parser a, exactly : P.Parser a, larger : Int -> P.Parser a, ending : P.Parser a } -> P.Parser a
indented indent next =
  let check actual =
        P.oneOf
          [ P.andThen (\_ -> next.ending) P.end
          , P.andThen (\_ -> next.ending) (P.symbol "\n...\n")
          , if actual == indent then next.exactly
            else if actual > indent then next.larger actual
            else next.smaller
          ]
  in
  P.succeed identity
    |. whitespace
    |= P.getCol
    |> P.andThen check
