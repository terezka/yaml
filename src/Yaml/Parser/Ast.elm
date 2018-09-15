module Yaml.Parser.Ast exposing (Value(..), Property, toString, fromString)



-- AST


{-| -}
type Value
  = String_ String
  | Float_ Float
  | Int_ Int
  | List_ (List Value)
  | Record_ (List Property)
  | Bool_ Bool
  | Null_


{-| -}
type alias Property =
  { name : String 
  , value : Value 
  }


{-| -}
fromString : String -> Value
fromString string =
  case String.toLower (String.trim string) of
    "" -> Null_
    "null" -> Null_
    "true" -> Bool_ True
    "false" -> Bool_ False
    other -> 
      case String.toInt other of
        Just int -> Int_ int
        Nothing ->
          case String.toFloat other of
            Just float -> Float_ float
            Nothing -> String_ other




-- DISPLAY


{-| -}
toString : Value -> String
toString value =
  case value of
    String_ string ->
      "\"" ++ string ++ "\""
    
    Float_ float ->
      String.fromFloat float ++ " (float)"
    
    Int_ int ->
      String.fromInt int ++ " (int)"
    
    List_ list ->
      "[ " ++ String.join ", " (List.map toString list) ++ " ]"
    
    Record_ properties ->
      "{ " ++ String.join ", " (List.map toStringProperty properties) ++ " }"

    Bool_ True ->
      "True (bool)"

    Bool_ False ->
      "False (bool)"
    
    Null_ ->
      "Null"


toStringProperty : Property -> String
toStringProperty { name, value } =
  name ++ ": " ++ toString value
