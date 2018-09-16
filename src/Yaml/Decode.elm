module Yaml.Decode exposing 
  ( Decoder, Error(..), fromString
  , string, bool, int, float
  , nullable, list
  , field, at
  , Value, value, null, fail, succeed
  , map, map2, map3
  )

{-|

Turn YAML values into Elm values. The library is structured the same way
as a `Json.Decode` in `elm/json`, so if you haven't worked with decoders 
before, reading through [the guide](https://guide.elm-lang.org/effects/json.html)
maybe be helpful.

@docs Decoder, Error, fromString

# Primitives
@docs string, bool, int, float

# Data Structures
@docs nullable, list

# Object Primitives
@docs field, at

# Maps
@docs map, map2, map3 

# Special
@docs Value, value, null, fail, succeed


-}

import Yaml.Parser as Yaml
import Yaml.Parser.Ast as Ast


{-| A value that knows how to decode YAML values.

There is a whole section in guide.elm-lang.org about decoders, 
so [check it out](https://guide.elm-lang.org/effects/json.html) 
for a more comprehensive introduction!

-}
type Decoder a =
  Decoder (Yaml.Value -> Result Error a)


{-| Represents a YAML value.
-}
type alias Value =
  Yaml.Value


{-| -}
type Error -- TODO
  = Parsing String
  | Decoding String


{-| -}
fromString : Decoder a -> String -> Result Error a
fromString decoder raw =
  case Yaml.fromString raw of
    Ok v -> fromValue decoder v
    Err error -> Err (Parsing "error parsing") -- TODO


fromValue : Decoder a -> Yaml.Value -> Result Error a
fromValue (Decoder decoder) v =
  decoder v



-- PRIMITIVES


{-| Decode a YAML string into an Elm `String`. 
-}
string : Decoder String
string =
  Decoder <| \v ->
    case v of
      Ast.String_ string_ -> Ok string_
      _ -> Err (Decoding "Expected string")


{-| Decode a YAML boolean into an Elm `Bool`.
-}
bool : Decoder Bool
bool =
  Decoder <| \v ->
    case v of
      Ast.Bool_ bool_ -> Ok bool_
      _ -> Err (Decoding "Expected bool")


{-| Decode a YAML number into an Elm `Int`.
-}
int : Decoder Int
int =
  Decoder <| \v ->
    case v of
      Ast.Int_ int_ -> Ok int_
      _ -> Err (Decoding "Expected int")


{-| Decode a YAML number into an Elm `Float`.
-}
float : Decoder Float
float =
  Decoder <| \v ->
    case v of
      Ast.Float_ float_ -> Ok float_
      _ -> Err (Decoding "Expected float")


{-| Decode a nullable YAML value into an Elm value.
-}
nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
  Decoder <| \v ->
    case v of
      Ast.Null_ -> Ok Nothing
      other -> Result.map Just (fromValue decoder other)


{-| Decode a YAML array into an Elm `List`.
-}
list : Decoder a -> Decoder (List a)
list decoder =
  Decoder <| \v ->
    case v of
      Ast.List_ list_ -> singleResult (List.map (fromValue decoder) list_)
      _ -> Err (Decoding "Expected list")


{-| Decode a YAML object, requiring a particular field.

The object can have other fields. Lots of them! The only thing this decoder 
cares about is if x is present and that the value there is an Int.

Check out [map2](#map2) to see how to decode multiple fields!

-}
field : String -> Decoder a -> Decoder a
field name decoder =
  Decoder <| \v ->
    find [ name ] decoder v


{-| Decode a nested YAML object, requiring certain fields. 
-}
at : List String -> Decoder a -> Decoder a
at names decoder =
  Decoder <| \v ->
    find names decoder v



-- SPECIAL


{-| Do not do anything with a YAML value, just bring it into 
Elm as a `Value`. This can be useful if you have particularly 
complex data that you would like to deal with later.
-}
value : Decoder Value
value =
  Decoder <| \v ->
    Ok v


{-| Decode a `null` value into some Elm value.
-}
null : a -> Decoder a
null default =
  Decoder <| \v ->
    case v of
      Ast.Null_ -> Ok default
      _ -> Err (Decoding "Expected null")


{-| Ignore the YAML and produce a certain Elm value.
-}
succeed : a -> Decoder a
succeed v =
  Decoder <| \_ ->
    Ok v


{-| Ignore the YAML and make the decoder fail. This is handy 
when used with `oneOf` or `andThen` where you want to give a 
custom error message in some case.

See the [andThen](#andThen) docs for an example.

-}
fail : String -> Decoder a
fail error =
  Decoder <| \_ ->
    Err (Decoding error)


{-| Create decoders that depend on previous results.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen next decoder =
  Decoder <| \v0 ->
    case fromValue decoder v0 of
      Ok a -> fromValue (next a) v0
      Err err -> Err err



-- MAPS


{-| Transform a decoder.
-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder a) =
  Decoder <| \v0 ->
    case a v0 of
      Err err -> Err err
      Ok av -> Ok (f av)

{-| Try two decoders and then combine the result.
-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f (Decoder a) (Decoder b) =
  Decoder <| \v0 ->
    case a v0 of
      Err err1 -> Err err1
      Ok av -> 
        case b v0 of
          Err err2 -> Err err2
          Ok bv -> Ok (f av bv)


{-| Try three decoders and then combine the result.
-}
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
find names decoder v0 =
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
      case v0 of
        Ast.Record_ properties -> 
          case findOne name properties of
            Ok v1 -> find rest decoder v1
            Err err -> Err err

        _ -> 
          Err (Decoding "Expected record")
      
    [] ->
      fromValue decoder v0
 

