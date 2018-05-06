module Ast.TopLevel exposing (..)

import Expect
import Test exposing (..)
import Yaml.Internal.Ast as Ast


topLevelArrays : Test
topLevelArrays =
    describe "An top level array"
        [ test "formatted" <|
            \_ ->
                let
                    expected =
                        Ast.Array
                            [ Ast.Primitive "a"
                            , Ast.Primitive "b"
                            , Ast.Primitive "c"
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        - a
                        - b
                        - c
                        """
        , test "with more than two spaces" <|
            \_ ->
                let
                    expected =
                        Ast.Array
                            [ Ast.Primitive "a"
                            , Ast.Primitive "b"
                            , Ast.Primitive "c"
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        - a
                        -   b
                        -  c
                        """
        , test "nested" <|
            \_ ->
                let
                    expected =
                        Ast.Array
                            [ Ast.Primitive "a"
                            , Ast.Array
                                [ Ast.Primitive "a"
                                , Ast.Primitive "b"
                                , Ast.Array
                                    [ Ast.Primitive "a"
                                    , Ast.Primitive "b"
                                    , Ast.Primitive "c"
                                    ]
                                ]
                            , Ast.Primitive "c"
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        - a
                        -
                            - a
                            - b
                            -
                                    - a
                                    - b
                                    - c
                        - c
                        """
        , test "with wrong indention" <|
            \_ ->
                Expect.err <|
                    Ast.build
                        """
                        - a
                         - b
                        - c
                        """
        , test "with wrong indention on second level" <|
            \_ ->
                Expect.err <|
                    Ast.build
                        """
                        - a
                        -
                            - a
                             - b
                            - c
                        - c
                        """
        , test "with hashes inside" <|
            \_ ->
                let
                    expected =
                        Ast.Array
                            [ Ast.Primitive "a"
                            , Ast.Hash
                                [ ( "b"
                                  , Ast.Hash
                                        [ ( "a", Ast.Primitive "1" )
                                        , ( "b", Ast.Primitive "2" )
                                        , ( "c", Ast.Primitive "3" )
                                        ]
                                  )
                                ]
                            , Ast.Primitive "c"
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        - a
                        - b:
                            a: 1
                            b: 2
                            c: 3
                        - c
                        """
        ]
