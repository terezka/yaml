module Ast exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Yaml.Internal.Ast as Ast


suite : Test
suite =
    describe "Yaml.Internal.Ast can parse"
        [ describe "A document begun declaration"
            [ test "when present" <|
                \_ ->
                    let
                        expected =
                            Ast.Primitive "hey"
                    in
                    Expect.equal (Ok expected) <|
                        Ast.build
                            """--- trash
                                hey
                                """
            , test "when missing" <|
                \_ ->
                    let
                        expected =
                            Ast.Primitive "hey"
                    in
                    Expect.equal (Ok expected) <|
                        Ast.build
                            """
                                hey
                                """
            ]
        ]
