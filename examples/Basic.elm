module Basic exposing (main)

import Html
import Yaml.Decode as Yaml


main : Html.Html msg
main =
    Html.text (toString test)


type alias Person =
    { name : String
    , age : Int
    , likesFruits : Bool
    , fruits : List String
    , details : Details
    }


type alias Details =
    { husband : Int
    , sister : Int
    }


test : Result String Person
test =
    Yaml.decodeString decoder
        """ name: Anezka Marie
            age: 25
            likesFruits: True
            fruits:
                - Apple
                - Orange
                - Pear
                - Mango
            details: {husband: 273688, sister: 87384245} """


decoder : Yaml.Decoder Person
decoder =
    Yaml.map5 Person
        (Yaml.field "name" Yaml.string)
        (Yaml.field "age" Yaml.int)
        (Yaml.field "likesFruits" Yaml.bool)
        (Yaml.field "fruits" (Yaml.list Yaml.string))
        (Yaml.field "details" decodeDetails)


decodeDetails : Yaml.Decoder Details
decodeDetails =
    Yaml.map2 Details
        (Yaml.field "husband" Yaml.int)
        (Yaml.field "sister" Yaml.int)
