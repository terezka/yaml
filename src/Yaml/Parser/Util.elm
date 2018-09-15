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
