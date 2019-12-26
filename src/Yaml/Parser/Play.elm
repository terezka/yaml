module Yaml.Parser.Play exposing (fromString)


import Parser exposing (..)
import Yaml.Parser.Ast as Ast
import Yaml.Parser.Util as U
import Yaml.Parser.Document
import Yaml.Parser.String
import Dict


{-| -}
fromString : String -> Result (List DeadEnd) String
fromString =
  run parser >> Result.map toString


type Data
  = Dollars Int
  | NotDollars String


toString : Data -> String 
toString data =
  case data of
    Dollars int -> String.fromInt int
    NotDollars string -> string


parser : Parser Data
parser =
  oneOf
    [ succeed identity
        |. symbol "$"
        |= oneOf
             [ succeed Dollars
                 |= int
             , succeed (\s -> NotDollars ("$" ++ s))
                 |= zeroOrMore (always True)
             ]
    , succeed NotDollars
        |= zeroOrMore (always True)
    ]

zeroOrMore : (Char -> Bool) -> Parser String
zeroOrMore isOk =
  succeed ()
    |. chompWhile isOk
    |> getChompedString

