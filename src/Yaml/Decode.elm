module Yaml.Decode exposing (Decoder, bool, decodeString, field, int, list, map, map2, map3, map4, map5, string)

{-|

@docs Decoder, decodeString, field, string, int, bool, list, map, map2, map3, map4, map5

-}

import Char
import Parser exposing ((|.), (|=), Parser, delayedCommit, ignore, keep, oneOrMore, symbol, zeroOrMore)


{-| -}
type Decoder a
    = Decoder (Parser a)


{-| -}
decodeString : Decoder a -> String -> Result String a
decodeString (Decoder parser) =
    Parser.run parser
        >> Result.mapError toString


{-| -}
field : String -> Decoder a -> Decoder a
field name (Decoder parser) =
    Decoder (parseField name parser)



-- PRIMITIVES


{-| -}
string : Decoder String
string =
    Decoder parseString


{-| -}
int : Decoder Int
int =
    Decoder Parser.int


{-| -}
bool : Decoder Bool
bool =
    Decoder parseBool


{-| -}
list : Decoder a -> Decoder (List a)
list (Decoder parser) =
    Decoder (parseList parser)



-- MAPS


{-| -}
map : (a -> b) -> Decoder a -> Decoder b
map func (Decoder a) =
    Decoder (Parser.map func a)


{-| -}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 func (Decoder a) (Decoder b) =
    Decoder (Parser.map2 func a b)


{-| -}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 func (Decoder a) (Decoder b) (Decoder c) =
    Decoder <|
        Parser.succeed func
            |= a
            |= b
            |= c


{-| -}
map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 func (Decoder a) (Decoder b) (Decoder c) (Decoder d) =
    Decoder <|
        Parser.succeed func
            |= a
            |= b
            |= c
            |= d


{-| -}
map5 : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
map5 func (Decoder a) (Decoder b) (Decoder c) (Decoder d) (Decoder e) =
    Decoder <|
        Parser.succeed func
            |= a
            |= b
            |= c
            |= d
            |= e



-- INTERNAL


parseField : String -> Parser a -> Parser a
parseField property parser =
    Parser.succeed identity
        |. parseSpaces
        |. symbol property
        |. symbol ":"
        |. parseSpaces
        |= parser
        |. parseNewLine


{-| TODO
-}
parseString : Parser String
parseString =
    keep oneOrMore (\c -> Char.isUpper c || Char.isLower c || Char.isDigit c || c == ' ')


parseBool : Parser Bool
parseBool =
    Parser.oneOf
        [ symbol "True" |> Parser.map (\_ -> True)
        , symbol "False" |> Parser.map (\_ -> False)
        ]


parseList : Parser a -> Parser (List a)
parseList parser =
    Parser.oneOf
        [ symbol "[]" |> Parser.map (\_ -> [])
        , parseListElements parser []
        ]


parseListElements : Parser a -> List a -> Parser (List a)
parseListElements parser revElements =
    Parser.oneOf
        [ parseListElement parser |> Parser.andThen (\n -> parseListElements parser (n :: revElements))
        , Parser.succeed (List.reverse revElements)
        ]


parseListElement : Parser a -> Parser a
parseListElement parser =
    delayedCommit parseSpaces <|
        Parser.succeed identity
            |. parseNewLine
            |. parseSpaces
            |. symbol "- "
            |= parser



-- INTERNAL / HELPERS


parseSpaces : Parser ()
parseSpaces =
    ignore zeroOrMore (\c -> c == ' ')


parseNewLine : Parser ()
parseNewLine =
    ignore zeroOrMore (\c -> String.fromChar c == "\n")
