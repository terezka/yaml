# YAML in Elm

This package helps you convert between Elm values and YAML values.

## Example

Say you have some YAML which looks like this:

```yaml
---
- name:
    first: Marie
    last: Curie
  occupation: [ chemist, physicist ]
  nationality: Polish
- name:
    first: Alva
    last: Myrdal
  occupation: [ sociologist, diplomat, politician ]
  nationality: Swedish
- name:
    first: Svetlana
    last: Alexievich
  occupation: [ journalist, historian ]
  nationality: Belarusian
...  
```

to decode this, you could write

```elm
module Woman exposing (Woman, decoder)

import Yaml.Decode

type alias Woman =
  { firstName : String
  , lastName : String
  , occupation : List String
  , nationality : String
  }

decoder : Yaml.Decode.Decoder Woman
decoder =
  Yaml.Decode.map4 Woman
    (Yaml.Decode.at [ "name", "first" ] Yaml.Decode.string)
    (Yaml.Decode.at [ "name", "last" ] Yaml.Decode.string)
    (Yaml.Decode.field "occupation" (Yaml.Decode.list Yaml.Decode.string))
    (Yaml.Decode.field "nationality" Yaml.Decode.string)

```

and run your decoder with `Yaml.Decode.fromString (Yaml.Decode.list Woman.decoder) yamlString`!

## Work in progress

This package was build to be able to parse data like [this](https://github.com/unitedstates/congress-legislators/blob/master/legislators-current.yaml), and even if it has a few more features (multiline strings, comments) than necessary to parse that file, YAML is a large and complex format, and this parser is still missing a lot of YAML features like references and various logical operations. It is also missing `Yaml.Encode`!
