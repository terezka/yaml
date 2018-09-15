module Main exposing (main)


import Browser
import Html
import Yaml.Parser
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
  Maybe (Result Issue Yaml.Parser.Value)


type Issue
  = HttpError Http.Error
  | ParserError (List Parser.DeadEnd)


init : () -> ( Model, Cmd Msg )
init _ =
  ( Nothing, fetchYaml )


type Msg
  = ReceiveYaml (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ReceiveYaml (Ok string) ->
      case Yaml.Parser.run string of
        Ok value ->
          ( Just (Ok value), Cmd.none )

        Err errors ->
          ( Just (Err (ParserError errors)), Cmd.none )

    ReceiveYaml (Err error) ->
      ( Just (Err (HttpError error)), Cmd.none )


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
        Err (HttpError error) ->
          Html.text <|
            case error of
              Http.BadUrl url -> "Bad url: " ++ url
              Http.Timeout -> "timeout"
              Http.NetworkError -> "NetworkError"
              Http.BadStatus _ -> "BadStatus"
              Http.BadPayload _ _ -> "BadPayload"

        Err (ParserError parserError) ->
          Html.text "parserError"

        Ok value ->
          yamlValueToHtml value


yamlValueToHtml : Yaml.Parser.Value -> Html.Html msg
yamlValueToHtml value =
  Html.text (Yaml.Parser.toString value)
