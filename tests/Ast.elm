module Ast exposing (..)

import Expect
import Test exposing (..)
import Yaml.Internal.Ast as Ast


documentBegun : Test
documentBegun =
    describe "A document begun declaration"
        [ test "when present" <|
            \_ ->
                let
                    expected =
                        Ast.Primitive "stuff"
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """--- trash
                        stuff
                        """
        , test "when missing" <|
            \_ ->
                let
                    expected =
                        Ast.Primitive "stuff"
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        stuff
                        """
        ]


strings : Test
strings =
    describe "A string"
        [ test "with spaces inside" <|
            \_ ->
                let
                    expected =
                        Ast.Primitive "stuff and more"
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        stuff and more
                        """
        , test "with } inside" <|
            \_ ->
                let
                    expected =
                        Ast.Primitive "stuff }and more"
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        stuff }and more
                        """
        , test "with ] inside" <|
            \_ ->
                let
                    expected =
                        Ast.Primitive "stuff ]and more"
                in
                Expect.equal (Ok expected) <|
                    Ast.build
                        """
                        stuff ]and more
                        """
        ]
