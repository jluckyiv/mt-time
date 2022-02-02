module ExamTests exposing (all)

import Expect
import Test exposing (..)
import WitnessExam exposing (..)


all : Test
all =
    describe "Test"
        [ test "encode exam" <|
            \_ ->
                Expect.equal
                    "{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}}"
                    (WitnessExam.empty |> WitnessExam.toJson)
        , test "decode exam value" <|
            \_ ->
                Expect.equal
                    (Ok WitnessExam.empty)
                    (WitnessExam.empty |> WitnessExam.encode |> WitnessExam.fromValue)
        , test "decode exam JSON" <|
            \_ ->
                Expect.equal
                    (Ok WitnessExam.empty)
                    ("{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}}"
                        |> WitnessExam.fromJson
                    )
        ]
