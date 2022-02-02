module Tests exposing (all)

import Duration exposing (..)
import Expect
import Seconds
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Test"
        [ describe "Duration"
            [ test "fromString 2:13" <|
                \_ ->
                    Expect.equal (Ok <| Duration.create 2 13) (Duration.fromString "2:13")
            , test "fromString, :13" <|
                \_ ->
                    Expect.equal (Ok <| Duration.create 0 13) (Duration.fromString ":13")
            , test "fromString, 0:13" <|
                \_ ->
                    Expect.equal (Ok <| Duration.create 0 13) (Duration.fromString "0:13")
            , test "fromString, 2:03" <|
                \_ ->
                    Expect.equal (Ok <| Duration.create 2 3) (Duration.fromString "2:03")
            , test "fromString, 0:03" <|
                \_ ->
                    Expect.equal (Ok <| Duration.create 0 3) (Duration.fromString "0:03")
            , test "fromString, 0:0" <|
                \_ ->
                    Expect.equal (Ok <| Duration.create 0 0) (Duration.fromString "0:0")
            , test "fromString, 213" <|
                \_ ->
                    Expect.equal (Ok <| Duration.create 2 13) (Duration.fromString "213")
            , test "fromString, 203" <|
                \_ ->
                    Expect.equal (Ok <| Duration.create 2 3) (Duration.fromString "203")
            , test "fromString, 003" <|
                \_ ->
                    Expect.equal (Ok <| Duration.create 0 3) (Duration.fromString "003")
            , test "fromString, \"\"" <|
                \_ ->
                    Expect.equal (Ok <| Duration.create 0 0) (Duration.fromString "")
            , test "toString" <|
                \_ ->
                    Expect.equal "2:13" (Duration.toString <| Duration.create 2 13)
            , test "toString, zero minutes" <|
                \_ ->
                    Expect.equal "0:13" (Duration.toString <| Duration.create 0 13)
            , test "toString, zero minutes and leading zero" <|
                \_ ->
                    Expect.equal "0:03" (Duration.toString <| Duration.create 0 3)
            , test "toString, zero minutes and zero seconds" <|
                \_ ->
                    Expect.equal "0:00" (Duration.toString <| Duration.create 0 0)
            , test "fromInt" <|
                \_ ->
                    Expect.equal (Duration.create 2 13) (Duration.fromInt 133)
            , test "toSeconds" <|
                \_ ->
                    Expect.equal 133 (Duration.create 2 13 |> Duration.toSeconds |> Seconds.toInt)
            , test "zero" <|
                \_ ->
                    Expect.equal (Duration.create 0 0) Duration.zero
            , test "EQ" <|
                \_ ->
                    Expect.equal (Duration.compare Duration.zero Duration.zero) EQ
            , test "GT" <|
                \_ ->
                    Expect.equal (Duration.compare (Duration.create 0 1) Duration.zero) GT
            , test "LT" <|
                \_ ->
                    Expect.equal (Duration.compare Duration.zero (Duration.create 0 1)) LT
            , test "add" <|
                \_ ->
                    Expect.equal (Duration.create 2 13) (Duration.add (Duration.create 2 0) (Duration.create 0 13))
            , test "subtract" <|
                \_ ->
                    Expect.equal (Just <| Duration.create 0 13) (Duration.subtract (Duration.create 2 0) (Duration.create 1 47))
            , test "remaining" <|
                \_ ->
                    Expect.equal
                        (Duration.create 1 27)
                        (Duration.remaining (Duration.create 5 0)
                            [ Duration.create 1 0
                            , Duration.create 1 0
                            , Duration.create 1 0
                            , Duration.create 0 33
                            ]
                        )
            , test "encode duration" <|
                \_ ->
                    Expect.equal
                        "{\"minutes\":1,\"seconds\":0}"
                        (Duration.create 1 0 |> Duration.toJson)
            , test "decode duration value" <|
                \_ ->
                    Expect.equal
                        (Ok (Duration.create 1 0))
                        (Duration.create 1 0 |> Duration.encode |> Duration.fromValue)
            , test "decode duration JSON" <|
                \_ ->
                    Expect.equal
                        (Ok (Duration.create 1 0))
                        ("{\"minutes\":1,\"seconds\":0}" |> Duration.fromJson)
            ]
        ]
