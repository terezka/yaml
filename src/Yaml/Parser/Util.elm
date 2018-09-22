module Yaml.Parser.Util exposing 
  ( isColon, isComma, isDot, isDash, isHash, isSpace, isNewLine, isListStart, isListEnd, isRecordStart, isRecordEnd, either, neither, neither3
  , colon, comma, dash, threeDashes, threeDots, space, spaces, newLine, newLines, whitespace, anything, multiline
  , anyOf
  , Branch, fork
  , singleQuotes, doubleQuotes, lineOfCharacters, characters, remaining
  , nextIndent, checkIndent, indented
  , propertyName
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



-- OTHER


{-| -}
anyOf : List Char -> P.Parser ()
anyOf endings =
  P.chompIf (\c -> List.member c endings)


{-| -}
end : P.Parser (a -> a)
end =
  P.oneOf
    [ P.succeed identity
        |. P.end
    , P.succeed identity
        |. threeDots
        |. whitespace
        |. P.end
    ]


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



-- STRINGS


{-| -}
character : Char -> P.Parser String
character char =
  P.succeed identity
    |. P.chompIf (\c -> c == char)
    |> P.getChompedString


{-| -}
charactersAny : List Char -> P.Parser String
charactersAny endings =
  P.succeed ()
    |. P.chompWhile (\c -> not (List.member c endings))
    |> P.getChompedString


{-| -}
characters : List Char -> P.Parser String
characters endings =
  let stringUntilComment = 
        P.succeed ()
          |. P.chompWhile (\c -> not (List.member c ('#' :: endings))) 
          |> P.getChompedString
  in
  P.succeed identity
    |= stringUntilComment
    |. P.oneOf 
        [ P.succeed () 
            |. comment
        , P.succeed () 
        ]


{-| -}
lineOfCharacters : P.Parser String
lineOfCharacters =
  let stringUntilComment = 
        P.succeed ()
          |. P.chompIf (\c -> c /= '\n')
          |. P.chompWhile (\c -> not (List.member c ['#', '\n'])) 
          |> P.getChompedString
  in
  P.succeed identity
    |= stringUntilComment
    |. P.oneOf 
        [ P.succeed () 
            |. comment
        , P.succeed () 
        ]


{-| -}
singleQuotes : P.Parser String
singleQuotes =
  P.succeed (String.replace "\\" "\\\\")
    |. P.symbol "'"
    |= charactersAny ['\'']
    |. P.symbol "'"
    |. spaces


{-| -}
doubleQuotes : P.Parser String
doubleQuotes =
  P.succeed identity
    |. P.symbol "\""
    |= charactersAny ['"']
    |. P.symbol "\""
    |. spaces


{-| -}
remaining : P.Parser String
remaining =
  P.succeed ()
    |. P.chompUntilEndOr "\n...\n"
    |> P.getChompedString


{-| -}
anything : P.Parser String
anything =
  P.succeed ()
    |. P.chompIf (always True)
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
          [ P.andThen (\_ -> next.ending) end
          , if actual == indent then next.exactly
            else if actual > indent then next.larger actual
            else next.smaller
          ]
  in
  P.andThen check nextIndent



{-| -}
indented : Int -> { smaller : P.Parser a, exactly : P.Parser a, larger : Int -> P.Parser a, ending : P.Parser a } -> P.Parser a
indented indent next =
  let check actual =
        P.oneOf
          [ P.andThen (\_ -> next.ending) end
          , if actual == indent then next.exactly
            else if actual > indent then next.larger actual
            else next.smaller
          ]
  in
  P.succeed identity
    |. whitespace
    |= P.getCol
    |> P.andThen check




-- FORK


type alias Branch a =
  { end : P.Parser ()
  , next : String -> P.Parser a
  }


fork : List (Branch a) -> P.Parser a
fork branches =
  P.loop [] (forkStep branches)
    |> P.andThen identity


forkStep : List (Branch a) -> List String -> P.Parser (P.Step (List String) (P.Parser a))
forkStep branches strings =
  let
    toDone next =
      P.Done (next (String.concat (List.reverse strings)))

    toMove string =
      P.Loop (string :: strings)

    toNext branch =
      P.succeed (\_ -> toDone branch.next)
        |= branch.end
  in
  P.oneOf
    [ P.oneOf (List.map toNext branches)
    , P.map toMove anything
    ]



-- PROPERTY NAME


{-| -}
propertyName : Bool -> P.Parser (Result Ast.Value String)
propertyName first =
  let append a b = a ++ b

      validateQuote name =
        P.oneOf
          [ P.succeed (Ok name)
              |. colon
          , P.succeed (Err name)
              |. newLine
          , P.succeed (Err name)
              |. end
          , P.problem "I was trying to parse a quoted string, but there was an unexpected _ directly afterwards!"
          ]

      validate name =
        P.oneOf
          [ P.succeed (Ok name)
              |. colon
          , P.succeed (Err << append name)
              |= if first 
                    then remaining
                    else characters ['\n'] -- TODO this string can also be multiline!
          ]
  in
  P.oneOf
    [ P.succeed identity
        |= singleQuotes
        |> P.andThen validateQuote
        |> P.map (Result.mapError Ast.String_)
    , P.succeed identity
        |= doubleQuotes
        |> P.andThen validateQuote
        |> P.map (Result.mapError Ast.String_)
    , P.succeed String.trim
        |= characters [':', '\n']
        |> P.andThen validate
        |> P.map (Result.mapError Ast.fromString)
    ]
