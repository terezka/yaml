module Yaml.Parser.Util exposing 
  ( colon, comma, dash, threeDashes, threeDots, space, spaces, newLine, newLines, whitespace
  , anyOf
  , singleQuotes, doubleQuotes, lineOfCharacters, characters, remaining
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
    |. P.oneOf [ comment, actualSpaces ]
    |. actualSpaces
  

{-| -}
whitespace : P.Parser ()
whitespace =
  P.succeed ()
    |. P.spaces
    |. P.oneOf [ comment, P.spaces ]
    |. P.spaces


{-| -}
newLines : P.Parser ()
newLines =
  let actualNewLines = 
        P.chompWhile (\c -> c == '\n')
  in
  P.succeed ()
    |. actualNewLines
    |. P.oneOf [ comment, actualNewLines ]
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
            |. if List.member '\n' endings then P.succeed () else newLine
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
  let
    addString ( result, ending ) string =
      if isThreeDots string then
        let final = result ++ ending in
        P.succeed (P.Done final) 
          |. anything

      else if isNewLine string then
        let next = ( result, ending ++ string ) in
        P.succeed (P.Loop next)

      else if isJustSpaces string then
        let next = ( result, ending ++ string ) in
        P.succeed (P.Loop next) 
          |. newLine

      else
        let next = ( result ++ ending ++ string, "" ) in
        P.succeed (P.Loop next) 
          |. newLine

    each result =
      P.andThen (addString result) <|
        P.oneOf 
          [ P.succeed "..." |. P.end 
          , P.succeed identity |= character '\n'
          , P.succeed identity |= lineOfCharacters
          ]
  in
  P.loop ( "","" ) each


anything : P.Parser String
anything =
  P.succeed ()
    |. P.chompWhile (always True)
    |> P.getChompedString


isThreeDots : String -> Bool
isThreeDots s =
  String.trimRight s == "..."


isNewLine : String -> Bool
isNewLine s =
  String.replace " " "" s == "\n"


isJustSpaces : String -> Bool
isJustSpaces s =
  String.isEmpty (String.replace " " "" s)



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
              |. spaces
              |. newLine
          , P.problem "I was trying to parse a quoted string, but there was an unexpected directly afterwards!"
          ]

      validate name =
        P.oneOf
          [ P.succeed (Ok name)
              |. colon
          , P.succeed (Err << append name)
              |= if first 
                    then remaining
                    else characters ['\n']
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
