module Ast.Inline exposing (..)

import Expect
import Test exposing (..)
import Yaml.Internal.Ast as Ast


inlineArrays : Test
inlineArrays =
    describe "An inline array"
        [ test "without spaces inside" <|
            \_ ->
                let
                    expected =
                        Ast.Array
                            [ Ast.Primitive "1"
                            , Ast.Primitive "2"
                            , Ast.Primitive "3"
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        [1,2,3]
                        """
        , test "with spaces inside" <|
            \_ ->
                let
                    expected =
                        Ast.Array
                            [ Ast.Primitive "1"
                            , Ast.Primitive "2"
                            , Ast.Primitive "3"
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        [ 1, 2, 3 ]
                        """
        , test "with irregular spaces inside" <|
            \_ ->
                let
                    expected =
                        Ast.Array
                            [ Ast.Primitive "1"
                            , Ast.Primitive "2"
                            , Ast.Primitive "3"
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        [ 1,2,   3 ]
                        """
        ]


inlineHashes : Test
inlineHashes =
    describe "An inline hash"
        [ test "formatted" <|
            \_ ->
                let
                    expected =
                        Ast.Hash
                            [ ( "a", Ast.Primitive "1" )
                            , ( "b", Ast.Primitive "2" )
                            , ( "c", Ast.Primitive "3" )
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        { a: 1, b: 2, c: 3 }
                        """
        , test "with irregular spaces inside" <|
            \_ ->
                let
                    expected =
                        Ast.Hash
                            [ ( "a", Ast.Primitive "1" )
                            , ( "b", Ast.Primitive "2" )
                            , ( "c", Ast.Primitive "3" )
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        {a:  1,  b:  2,c:  3    }
                        """
        , test "with no spaces inside" <|
            \_ ->
                let
                    expected =
                        Ast.Hash
                            [ ( "a", Ast.Primitive "1" )
                            , ( "b", Ast.Primitive "2" )
                            , ( "c", Ast.Primitive "3" )
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        {a:1,b:2,c:3}
                        """
        , test "with slashes and dots in the names" <|
            \_ ->
                let
                    expected =
                        Ast.Hash
                            [ ( "/etc/http/conf/http.conf", Ast.Primitive "1" )
                            , ( "b", Ast.Primitive "2" )
                            , ( "c", Ast.Primitive "3" )
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        { /etc/http/conf/http.conf: 1, b: 2, c: 3 }
                        """
        ]


inlineNesting : Test
inlineNesting =
    describe "An nested inlining"
        [ test "of four arrays" <|
            \_ ->
                let
                    expected =
                        Ast.Array
                            [ Ast.Primitive "1"
                            , Ast.Array
                                [ Ast.Array
                                    [ Ast.Primitive "1"
                                    , Ast.Primitive "2"
                                    , Ast.Array
                                        [ Ast.Primitive "1"
                                        , Ast.Primitive "2"
                                        , Ast.Primitive "3"
                                        ]
                                    ]
                                , Ast.Primitive "2"
                                , Ast.Primitive "3"
                                ]
                            , Ast.Primitive "3"
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        [1, [[1, 2, [1, 2, 3]], 2, 3], 3]
                        """
        , test "of four hashes" <|
            \_ ->
                let
                    expected =
                        Ast.Hash
                            [ ( "a"
                              , Ast.Hash
                                    [ ( "a", Ast.Primitive "1" )
                                    , ( "b"
                                      , Ast.Hash
                                            [ ( "a", Ast.Primitive "1" )
                                            , ( "b", Ast.Primitive "2" )
                                            , ( "c"
                                              , Ast.Hash
                                                    [ ( "a", Ast.Primitive "1" )
                                                    , ( "b", Ast.Primitive "2" )
                                                    , ( "c", Ast.Primitive "3" )
                                                    ]
                                              )
                                            ]
                                      )
                                    , ( "c", Ast.Primitive "3" )
                                    ]
                              )
                            , ( "b", Ast.Primitive "2" )
                            , ( "c", Ast.Primitive "3" )
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        {a:{a:1,b:{a:1,b:2,c:{a:1,b:2,c:3}},c:3},b:2,c:3}
                        """
        , test "of hashes inside arrays" <|
            \_ ->
                let
                    expected =
                        Ast.Array
                            [ Ast.Primitive "1"
                            , Ast.Hash
                                [ ( "a", Ast.Primitive "1" )
                                , ( "b", Ast.Primitive "2" )
                                , ( "c", Ast.Primitive "3" )
                                ]
                            , Ast.Primitive "2"
                            , Ast.Primitive "3"
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        [ 1, {a:1,b:2,c:3}, 2, 3 ]
                        """
        , test "of arrays inside hashes" <|
            \_ ->
                let
                    expected =
                        Ast.Hash
                            [ ( "a", Ast.Primitive "1" )
                            , ( "b"
                              , Ast.Array
                                    [ Ast.Primitive "1"
                                    , Ast.Primitive "2"
                                    , Ast.Primitive "3"
                                    ]
                              )
                            , ( "c", Ast.Primitive "3" )
                            ]
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        {a:1,b:[ 1, 2, 3 ],c:3}
                        """
        ]
