module Yaml.Decode exposing (Decoder, Error(..), fromString, string, bool, int, float, list, field, at, map, map2, map3)

{-| @docs Decoder, Error, fromString, string, bool, int, float, list, field, at, map, map2, map3 -}

import Yaml.Parser as Yaml
import Yaml.Parser.Ast as Ast


{-| -}
type Decoder a =
  Decoder (Yaml.Value -> Result Error a)


{-| -}
type Error -- TODO
  = Parsing String
  | Decoding String


{-| -}
fromString : Decoder a -> String -> Result Error a
fromString decoder raw =
  case Yaml.fromString raw of
    Ok value -> fromValue decoder value
    Err error -> Err (Parsing "error parsing") -- TODO


fromValue : Decoder a -> Yaml.Value -> Result Error a
fromValue (Decoder decoder) value =
  decoder value


{-| -}
string : Decoder String
string =
  Decoder <| \value ->
    case value of
      Ast.String_ string_ -> Ok string_
      _ -> Err (Decoding "Expected string")


{-| -}
bool : Decoder Bool
bool =
  Decoder <| \value ->
    case value of
      Ast.Bool_ bool_ -> Ok bool_
      _ -> Err (Decoding "Expected bool")


{-| -}
int : Decoder Int
int =
  Decoder <| \value ->
    case value of
      Ast.Int_ int_ -> Ok int_
      _ -> Err (Decoding "Expected int")


{-| -}
float : Decoder Float
float =
  Decoder <| \value ->
    case value of
      Ast.Float_ float_ -> Ok float_
      _ -> Err (Decoding "Expected float")


{-| -}
list : Decoder a -> Decoder (List a)
list decoder =
  Decoder <| \value ->
    case value of
      Ast.List_ list_ -> singleResult (List.map (fromValue decoder) list_)
      _ -> Err (Decoding "Expected list")


{-| -}
field : String -> Decoder a -> Decoder a
field name decoder =
  Decoder <| \value ->
    find [ name ] decoder value


{-| -}
at : List String -> Decoder a -> Decoder a
at names decoder =
  Decoder <| \value ->
    find names decoder value


{-| -}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder a) =
  Decoder <| \v0 ->
    case a v0 of
      Err err -> Err err
      Ok av -> Ok (f av)

{-| -}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f (Decoder a) (Decoder b) =
  Decoder <| \v0 ->
    case a v0 of
      Err err1 -> Err err1
      Ok av -> 
        case b v0 of
          Err err2 -> Err err2
          Ok bv -> Ok (f av bv)


{-| -}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 f (Decoder a) (Decoder b) (Decoder c) =
  Decoder <| \v0 ->
    case a v0 of
      Err err1 -> Err err1
      Ok av -> 
        case b v0 of
          Err err2 -> Err err2
          Ok bv ->
            case c v0 of
              Err err3 -> Err err3
              Ok cv -> Ok (f av bv cv)



-- INTERNAL


singleResult : List (Result Error a) -> Result Error (List a)
singleResult =
  let
    each v r =
      case r of
        Err _ -> r
        Ok vs ->
          case v of
            Ok vok -> Ok (vok :: vs)
            Err err -> Err err
  in
  List.foldl each (Ok []) >> Result.map List.reverse


find : List String -> Decoder a -> Ast.Value -> Result Error a
find names decoder value =
  let
    findOne name properties =
      properties
        |> List.filter (\p -> p.name == name)
        |> List.head
        |> Maybe.map (Ok << .value)
        |> Maybe.withDefault (Err (Decoding <| "Expected property: " ++ name))
  in
  case names of 
    name :: rest -> 
      case value of
        Ast.Record_ properties -> 
          case findOne name properties of
            Ok v -> find rest decoder v
            Err err -> Err err

        v -> Err (Decoding "Expected record")
      
    [] ->
      fromValue decoder value
 

