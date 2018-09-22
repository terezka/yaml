module Main exposing (..)

import Expect
import Test
import Yaml.Parser as Parser
import Yaml.Parser.Ast as Ast
import Dict


suite : Test.Test
suite =
  Test.describe "The parser"
    [ Test.test "nothing" <|
        \_ -> 
          expectValue "" <|
            Ast.Null_
    ,  Test.test "nothing with document start" <|
        \_ -> 
          expectValue "---" <|
            Ast.Null_
    ,  Test.test "nothing with document end" <|
        \_ -> 
          expectValue "..." <|
            Ast.Null_
    ,  Test.test "nothing with document start and end" <|
        \_ -> -- TODO is this the right response?
          expectValue "---..." <|
            Ast.Null_
    , Test.test "null" <|
        \_ -> 
          expectValue "" <|
            Ast.Null_
    , Test.test "an int" <|
        \_ -> 
          expectValue "0" <| 
            Ast.Int_ 0
    , Test.test "a float" <|
        \_ -> 
          expectValue "0.5" <| 
            Ast.Float_ 0.5
    , Test.test "a single-quoted string" <|
        \_ -> -- TODO is this right?
          expectValue """'hey 
          i am a \n
          parser'""" <| 
            Ast.String_ "hey \n          i am a \n\n          parser"
    , Test.test "a double-quoted string" <|
        \_ -> 
          expectValue """"hey 
          i am a 
          parser" """ <| 
            Ast.String_ "hey \n          i am a \n          parser"
    , Test.test "a single-line string" <|
        \_ -> 
          expectValue "hey i am a parser" <| 
            Ast.String_ "hey i am a parser"
    , Test.test "a multi-line string" <|
        \_ -> 
          expectValue
            """how does one teach self-respect?
            how does one teach curiousity?
            if you have the answers, call me
            """ <| 
            Ast.String_ "how does one teach self-respect?\n            how does one teach curiousity?\n            if you have the answers, call me"
    , Test.test "a multi-line string with three dot ending" <|
        \_ -> 
          expectValue
            """how does one teach self-respect?
            how does one teach curiousity?
            if you have the answers, call me
...



            """ <| 
            Ast.String_ "how does one teach self-respect?\n            how does one teach curiousity?\n            if you have the answers, call me"
    , Test.test "a empty inline list" <|
        \_ -> 
          expectValue "[]" <|
            Ast.List_ []
    , Test.test "an inline list with ints and no spaces" <|
        \_ -> 
          expectValue "[0,1,2]" <|
            Ast.List_ [ Ast.Int_ 0, Ast.Int_ 1, Ast.Int_ 2 ]
    , Test.test "an inline list with ints and with spaces" <|
        \_ -> 
          expectValue "[ 0, 1, 2 ]" <|
            Ast.List_ [ Ast.Int_ 0, Ast.Int_ 1, Ast.Int_ 2 ]
    , Test.test "an inline list with strings and with spaces" <|
        \_ -> 
          expectValue "[ aaa, bbb, ccc ]" <|
            Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ]
    , Test.test "an inline list with strings and with new lines" <|
        \_ -> 
          expectValue """[ aaa,
           bbb, 
           ccc 
           ]""" <|
            Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ]
    , Test.test "an inline list with strings and with new lines and comments" <|
        \_ -> 
          expectValue """[ aaa,
           bbb, # a dumb comment
           ccc 
           ]""" <|
            Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ]
    , Test.test "an inline list with strings, with new lines, and multi-line strings" <|
        \_ -> 
          expectValue """[ aaa,
           bbb, 
           ccc ccc
           ccc 
           ]""" <|
            Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc ccc\n           ccc" ]
    , Test.test "an inline list with an inline list inside" <|
        \_ -> 
          expectValue "[ aaa, [bbb, aaa, ccc], ccc ]" <|
            Ast.List_ [ Ast.String_ "aaa", Ast.List_ [ Ast.String_ "bbb", Ast.String_ "aaa", Ast.String_ "ccc" ], Ast.String_ "ccc" ]
    , Test.test "an inline list with a record inside" <|
        \_ -> 
          expectValue "[ aaa, {bbb: bbb, aaa: aaa, ccc: ccc}, ccc ]" <|
            Ast.List_ [ Ast.String_ "aaa", Ast.Record_ (Dict.fromList [ ("bbb", Ast.String_ "bbb"), ("aaa", Ast.String_ "aaa"), ("ccc", Ast.String_ "ccc") ]), Ast.String_ "ccc" ]
    , Test.test "an inline record" <|
        \_ -> 
          expectValue "{bbb: bbb, aaa: aaa, ccc: ccc}" <|
            Ast.Record_ (Dict.fromList [ ("bbb", Ast.String_ "bbb"), ("aaa", Ast.String_ "aaa"), ("ccc", Ast.String_ "ccc") ])
    , Test.test "an inline record with weird spacing" <|
        \_ -> 
          expectValue 
            """
            {bbb:
             bbb , aaa: aaa
                  , ccc: ccc}
            """ <|
            Ast.Record_ (Dict.fromList [ ("bbb", Ast.String_ "bbb"), ("aaa", Ast.String_ "aaa"), ("ccc", Ast.String_ "ccc") ])
    , Test.test "an inline record with an inline record inside" <|
        \_ -> 
          expectValue "{bbb: bbb, aaa: {bbb: bbb, aaa: aaa, ccc: ccc}, ccc: ccc}" <|
            Ast.Record_ (Dict.fromList [ ("bbb", Ast.String_ "bbb"), ("aaa", Ast.Record_ (Dict.fromList [ ("bbb", Ast.String_ "bbb"), ("aaa", Ast.String_ "aaa"), ("ccc", Ast.String_ "ccc") ])), ("ccc", Ast.String_ "ccc") ])
    , Test.test "a list" <|
        \_ -> 
          expectValue 
            """
            - aaa
            - bbb
            - ccc
            """ <|
            Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ]
    , Test.test "a list with null" <|
      \_ -> 
        expectValue 
          """
          -
          - bbb
          - ccc
          """ <|
          Ast.List_ [ Ast.Null_, Ast.String_ "bbb", Ast.String_ "ccc" ]
    , Test.test "a list with multi-line string" <|
      \_ -> 
        expectValue 
          """
          -
          - bbb
           bbb bbb
           bbb
          - ccc
          """ <|
            Ast.List_ [ Ast.Null_, Ast.String_ "bbb\nbbb bbb\nbbb", Ast.String_ "ccc" ]
    , Test.test "a list with a list inside" <|
      \_ -> 
        expectValue """
          - aaa
          -
            - aaa
            - bbb
            - ccc
          - ccc
          """ <|
          Ast.List_ [ Ast.String_ "aaa", Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ], Ast.String_ "ccc" ]
    , Test.test "a list with a list inside on same line" <|
      \_ -> 
        expectValue """
          - aaa
          - - aaa
            - bbb
            - ccc
          - ccc
          """ <|
          Ast.List_ [ Ast.String_ "aaa", Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ], Ast.String_ "ccc" ]
    , Test.test "a record" <|
      \_ -> 
        expectValue 
          """
          aaa: aaa
          bbb: bbb
          ccc: ccc
          """ <|
          Ast.Record_ (Dict.fromList [ ("aaa", Ast.String_ "aaa"), ("bbb", Ast.String_ "bbb"), ("ccc", Ast.String_ "ccc") ])
    , Test.test "a record with quoted property names and values" <|
        \_ -> 
          expectValue 
            """
            'aaa': aaa
            "bbb": "bbb"
            'ccc': 'ccc'
            """ <|
            Ast.Record_ (Dict.fromList [ ("aaa", Ast.String_ "aaa"), ("bbb", Ast.String_ "bbb"), ("ccc", Ast.String_ "ccc") ])
    , Test.test "a record with a record inside" <|
        \_ -> 
          expectValue 
            """
            aaa: aaa
            bbb:
              aaa: aaa
              bbb: bbb
              ccc: ccc
            ccc: ccc
            """ <|
            Ast.Record_ (Dict.fromList [ ("aaa", Ast.String_ "aaa"), ("bbb", Ast.Record_ (Dict.fromList [ ("aaa", Ast.String_ "aaa"), ("bbb", Ast.String_ "bbb"), ("ccc", Ast.String_ "ccc") ])), ("ccc", Ast.String_ "ccc") ])
    , Test.test "a record with a list inside" <|
        \_ -> 
          expectValue 
            """
            aaa: aaa
            bbb:
              - aaa
              - bbb
              - ccc
            ccc: ccc
            """ <|
            Ast.Record_ (Dict.fromList [ ("aaa", Ast.String_ "aaa"), ("bbb", Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ]), ("ccc", Ast.String_ "ccc") ])
    , Test.test "a record with a list inside on the same level" <|
        \_ -> 
          expectValue 
            """
            aaa: aaa
            bbb:
            - aaa
            - bbb
            - ccc
            ccc: ccc
            """ <|
            Ast.Record_ (Dict.fromList [ ("aaa", Ast.String_ "aaa"), ("bbb", Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ]), ("ccc", Ast.String_ "ccc") ])
    , Test.test "a record with comments" <|
        \_ -> 
          expectValue 
            """
            aaa: aaa # hey
            bbb: bbb
            ccc: ccc
            """ <|
            Ast.Record_ (Dict.fromList [ ("aaa", Ast.String_ "aaa"), ("bbb", Ast.String_ "bbb"), ("ccc", Ast.String_ "ccc") ])
    ]


expectValue : String -> Ast.Value -> Expect.Expectation
expectValue subject expected =
  Parser.fromString subject
    |> Expect.equal (Ok expected)
 
 
 
 
