module Yaml.Parser.Record exposing (Toplevel, toplevel, Inline, inline)


import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Util as U
import Yaml.Parser.Ast as Ast
import Yaml.Parser.Null



-- TOPLEVEL


{-| -}
type alias Toplevel =
  { childInline : P.Parser Ast.Value
  , childToplevel : P.Parser Ast.Value
  , list : Int -> P.Parser Ast.Value
  }


{-| -}
toplevel : Toplevel -> Bool -> Int -> P.Parser Ast.Value
toplevel config first indent =
  let
    withProperty name =
      case name of 
        Ok validName -> toplevelConfirmed config indent validName
        Err value -> P.succeed value
  in
  U.propertyName first 
    |> P.andThen withProperty


toplevelConfirmed : Toplevel -> Int -> String -> P.Parser Ast.Value
toplevelConfirmed config indent name =
  let
    withValue value =
      P.succeed Ast.Record_
        |= P.loop [ Ast.Property name value ] (toplevelEach config indent)
  in
  config.childInline
    |> P.andThen withValue


toplevelEach : Toplevel -> Int -> List Ast.Property -> P.Parser (P.Step (List Ast.Property) (List Ast.Property))
toplevelEach config indent properties =
  let finish = P.Done (List.reverse properties)
      next property = P.Loop (property :: properties)
      continued = P.Loop

      toplevelNext =
        P.oneOf 
          [ P.map next (toplevelNewEntry config)
          , P.succeed finish 
          ]

      toplevelNextOrList latest rest =
        P.oneOf 
          [ P.map (\list -> continued ({ latest | value = list } :: rest)) (config.list indent)
          , P.map next (toplevelNewEntry config)
          , P.succeed finish 
          ]
  in
  U.checkIndent indent
    { smaller = P.succeed finish
    , exactly = 
        case properties of 
          latest :: rest ->
            case latest.value of
              Ast.Null_ ->
                -- Lists are allowed on the same level as the record
                toplevelNextOrList latest rest
                
              _ ->
                toplevelNext

          _ ->
            toplevelNext
    , larger = P.map continued << toplevelContinuedEntry config properties
    , ending = P.succeed finish
    }


toplevelNewEntry : Toplevel -> P.Parser Ast.Property
toplevelNewEntry config =
  let
    withProperty name =
      case name of 
        Ok validName ->
          P.map (Ast.Property validName) <|
            P.oneOf 
              [ Yaml.Parser.Null.inline
              , P.succeed identity
                  |. U.space
                  |. U.spaces
                  |= config.childInline
              ]

        Err _ -> 
          toplevelMissingColon
  in
  U.propertyName False |> P.andThen withProperty


toplevelContinuedEntry : Toplevel -> List Ast.Property -> Int -> P.Parser (List Ast.Property)
toplevelContinuedEntry config properties subIndent =
  let
    coalesce value =
      case properties of
        latest :: rest ->
          case ( latest.value, value ) of
            ( Ast.Null_, _ ) -> 
              P.succeed ({ latest | value = value } :: rest)

            ( Ast.String_ prev, Ast.String_ new ) -> -- TODO don't skip new lines
              P.succeed ({ latest | value = Ast.String_ (prev ++ " " ++ new) } :: rest)

            ( _, _ ) ->
              toplevelMissingProperty value

        rest ->
          toplevelMissingProperty value
  in
  P.andThen coalesce config.childToplevel


toplevelMissingColon : P.Parser a
toplevelMissingColon =
  P.problem "I was parsing a record, but I couldn't find the \":\"!"


toplevelMissingProperty : Ast.Value -> P.Parser a
toplevelMissingProperty value =
  P.problem <|
    "I was parsing a record and was expecting a new property, but instead I got " ++
      case value of
        Ast.Null_ -> "a null"
        Ast.String_ string -> "a string (" ++ string ++ ")"
        Ast.Record_ record -> "another record!"
        Ast.List_ list -> "a list!"
        Ast.Int_ list -> "an int!"
        Ast.Float_ list -> "a float!"



-- INLINE


{-| -}
type alias Inline =
  { inline : List Char -> P.Parser Ast.Value  }


{-| -}
inline : Inline -> P.Parser Ast.Value
inline config =
  P.succeed Ast.Record_
    |. P.symbol "{"
    |. U.spaces
    |= P.oneOf
        [ P.succeed [] |. P.symbol "}"
        , P.loop [] (inlineEach config)
        ]


inlineEach : Inline -> List Ast.Property -> P.Parser (P.Step (List Ast.Property) (List Ast.Property))
inlineEach config properties =
  let
    withProperty name =
      case name of 
        Ok validName ->
          P.succeed (\v next -> next (Ast.Property validName v :: properties))
            |= inlineValue config
            |= P.oneOf
                [ P.succeed P.Loop |. U.comma
                , P.succeed (P.Done << List.reverse) |. P.symbol "}"
                ]
            |. U.spaces

        Err _ -> 
          errorMissingColon
  in
  U.propertyName False 
    |> P.andThen withProperty


inlineValue : Inline -> P.Parser Ast.Value
inlineValue config =
  P.oneOf
    [ P.succeed identity
        |. P.oneOf [ U.space, U.newLine ]
        |. U.whitespace -- TODO ?
        |= config.inline [',', '}']
        |. U.spaces
    , P.succeed ()
        |. P.chompIf (\c -> c /= ',' && c /= '}' && c /= '\n')
        |> P.andThen (\_ -> errorMissingSpaceAfterColon)
    , P.succeed Ast.Null_
    ]


errorMissingColon : P.Parser a 
errorMissingColon =
  P.problem "I was parsing an inline record, but I couldn't find the \":\"!"


errorMissingSpaceAfterColon : P.Parser a 
errorMissingSpaceAfterColon =
  P.problem "I was parsing an inline record, but missing a space between the \":\" and the value!"

