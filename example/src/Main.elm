module Main exposing (main)


import Browser
import Html
import Yaml.Parser
import Yaml.Decode
import Http
import Parser


main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


type alias Model =
  Maybe (Result Issue (List Person))


type Issue
  = Fetching Http.Error
  | Decoding Yaml.Decode.Error


init : () -> ( Model, Cmd Msg )
init _ =
  ( Nothing, fetchYaml )


type Msg
  = ReceiveYaml (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ReceiveYaml (Ok string) ->
      case Yaml.Decode.fromString decoder string of
        Ok value ->
          ( Just (Ok value), Cmd.none )

        Err error ->
          ( Just (Err (Decoding error)), Cmd.none )

    ReceiveYaml (Err error) ->
      ( Just (Err (Fetching error)), Cmd.none )


fetchYaml : Cmd Msg
fetchYaml =
  "https://cdn.rawgit.com/unitedstates/congress-legislators/master/legislators-current.yaml"
    |> Http.getString
    |> Http.send ReceiveYaml



view : Model -> Html.Html Msg
view model =
  case model of
    Nothing ->
      Html.text "loading"

    Just result ->
      case result of 
        Err (Fetching error) ->
          Html.text <|
            case error of
              Http.BadUrl url -> "Bad url: " ++ url
              Http.Timeout -> "timeout"
              Http.NetworkError -> "NetworkError"
              Http.BadStatus _ -> "BadStatus"
              Http.BadPayload _ _ -> "BadPayload"

        Err (Decoding error) ->
          Html.text <|
            case error of
              Yaml.Decode.Parsing string -> string
              Yaml.Decode.Decoding string -> string

        Ok people ->
          Html.div [] (List.map viewPerson (List.sortBy (.religion >> Maybe.withDefault "z") people))


viewPerson : Person -> Html.Html msg
viewPerson person =
  Html.div [] 
    [ -- Html.div [] [ Html.text ("Name: " ++ person.name) ] 
    -- , 
    Html.div [] [ Html.text ("Religion: " ++ Maybe.withDefault "unspecified" person.religion) ] 
    -- , Html.div [] [ Html.text ("Terms: " ++ String.join ", " person.terms) ]
    ]



-- DECODER 


type alias Person =
  { name : String
  , religion : Maybe String
  , terms : List String 
  }


decoder : Yaml.Decode.Decoder (List Person)
decoder =
  Yaml.Decode.list decodePerson


decodePerson : Yaml.Decode.Decoder Person
decodePerson =
  Yaml.Decode.map3 Person
    (Yaml.Decode.at ["name", "last"] Yaml.Decode.string)
    (Yaml.Decode.field "bio" decodeBio)
    (Yaml.Decode.field "terms" (Yaml.Decode.list decodeTerm))


decodeBio : Yaml.Decode.Decoder (Maybe String)
decodeBio =
  Yaml.Decode.sometimes <|
    Yaml.Decode.field "religion" Yaml.Decode.string


decodeTerm : Yaml.Decode.Decoder String
decodeTerm =
  Yaml.Decode.field "start" Yaml.Decode.string


