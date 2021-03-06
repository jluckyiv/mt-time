module Tests exposing (all)

import Duration exposing (..)
import Exam exposing (..)
import ExamCollection exposing (..)
import Expect
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Duration"
        [ test "fromString 2:13" <|
            \_ ->
                Expect.equal (Ok <| Duration 2 13) (Duration.fromString "2:13")
        , test "fromString, :13" <|
            \_ ->
                Expect.equal (Ok <| Duration 0 13) (Duration.fromString ":13")
        , test "fromString, 0:13" <|
            \_ ->
                Expect.equal (Ok <| Duration 0 13) (Duration.fromString "0:13")
        , test "fromString, 2:03" <|
            \_ ->
                Expect.equal (Ok <| Duration 2 3) (Duration.fromString "2:03")
        , test "fromString, 0:03" <|
            \_ ->
                Expect.equal (Ok <| Duration 0 3) (Duration.fromString "0:03")
        , test "fromString, 0:0" <|
            \_ ->
                Expect.equal (Ok <| Duration 0 0) (Duration.fromString "0:0")
        , test "fromString, 213" <|
            \_ ->
                Expect.equal (Ok <| Duration 2 13) (Duration.fromString "213")
        , test "fromString, 203" <|
            \_ ->
                Expect.equal (Ok <| Duration 2 3) (Duration.fromString "203")
        , test "fromString, 003" <|
            \_ ->
                Expect.equal (Ok <| Duration 0 3) (Duration.fromString "003")
        , test "fromString, \"\"" <|
            \_ ->
                Expect.equal (Ok <| Duration 0 0) (Duration.fromString "")
        , test "toString" <|
            \_ ->
                Expect.equal "2:13" (Duration.toString <| Duration 2 13)
        , test "toString, zero minutes" <|
            \_ ->
                Expect.equal "0:13" (Duration.toString <| Duration 0 13)
        , test "toString, zero minutes and leading zero" <|
            \_ ->
                Expect.equal "0:03" (Duration.toString <| Duration 0 3)
        , test "toString, zero minutes and zero seconds" <|
            \_ ->
                Expect.equal "0:00" (Duration.toString <| Duration 0 0)
        , test "fromSeconds" <|
            \_ ->
                Expect.equal (Duration 2 13) (Duration.fromSeconds 133)
        , test "toSeconds" <|
            \_ ->
                Expect.equal 133 (Duration.toSeconds <| Duration 2 13)
        , test "zero" <|
            \_ ->
                Expect.equal (Duration 0 0) Duration.empty
        , test "add" <|
            \_ ->
                Expect.equal (Duration 2 13) (Duration.add (Duration 2 0) (Duration 0 13))
        , test "subtract" <|
            \_ ->
                Expect.equal (Duration 0 13) (Duration.subtract (Duration 2 0) (Duration 1 47))
        , test "remaining" <|
            \_ ->
                Expect.equal
                    (Duration 1 27)
                    (Duration.remaining (Duration 5 0)
                        [ Duration 1 0
                        , Duration 1 0
                        , Duration 1 0
                        , Duration 0 33
                        ]
                    )
        , test "encode duration" <|
            \_ ->
                Expect.equal
                    "{\"minutes\":1,\"seconds\":0}"
                    (Duration 1 0 |> Duration.toJson)
        , test "decode duration value" <|
            \_ ->
                Expect.equal
                    (Ok (Duration 1 0))
                    (Duration 1 0 |> Duration.encodeDuration |> Duration.fromValue)
        , test "decode duration JSON" <|
            \_ ->
                Expect.equal
                    (Ok (Duration 1 0))
                    ("{\"minutes\":1,\"seconds\":0}" |> Duration.fromJson)
        , test "encode exam" <|
            \_ ->
                Expect.equal
                    "{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}}"
                    (Exam.empty |> Exam.toJson)
        , test "decode exam value" <|
            \_ ->
                Expect.equal
                    (Ok Exam.empty)
                    (Exam.empty |> Exam.encodeExam |> Exam.fromValue)
        , test "decode exam JSON" <|
            \_ ->
                Expect.equal
                    (Ok Exam.empty)
                    ("{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}}"
                        |> Exam.fromJson
                    )
        , test "encode collection" <|
            \_ ->
                Expect.equal
                    "{\"P1\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P2\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P3\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P4\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}}}"
                    (ExamCollection.new [ "P1", "P2", "P3", "P4" ] |> ExamCollection.toJson)
        , test "decode collection value" <|
            \_ ->
                Expect.equal
                    (Ok (ExamCollection.new [ "P1", "P2", "P3", "P4" ]))
                    (ExamCollection.new [ "P1", "P2", "P3", "P4" ] |> ExamCollection.encodeCollection |> ExamCollection.fromValue)
        , test "decode collection JSON" <|
            \_ ->
                Expect.equal
                    (Ok (ExamCollection.new [ "P1", "P2", "P3", "P4" ]))
                    ("{\"P1\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P2\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P3\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P4\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}}}"
                        |> ExamCollection.fromJson
                    )
        ]
