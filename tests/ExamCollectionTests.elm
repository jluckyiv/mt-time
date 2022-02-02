module ExamCollectionTests exposing (all)

import ExamCollection exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "Test"
        [ test "encode collection" <|
            \_ ->
                Expect.equal
                    "{\"P1\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P2\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P3\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P4\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}}}"
                    (ExamCollection.new [ "P1", "P2", "P3", "P4" ] |> ExamCollection.toJson)
        , test "decode collection value" <|
            \_ ->
                Expect.equal
                    (Ok (ExamCollection.new [ "P1", "P2", "P3", "P4" ]))
                    (ExamCollection.new [ "P1", "P2", "P3", "P4" ] |> ExamCollection.encode |> ExamCollection.fromValue)
        , test "decode collection JSON" <|
            \_ ->
                Expect.equal
                    (Ok (ExamCollection.new [ "P1", "P2", "P3", "P4" ]))
                    ("{\"P1\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P2\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P3\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}},\"P4\":{\"direct\":{\"minutes\":0,\"seconds\":0},\"cross\":{\"minutes\":0,\"seconds\":0},\"redirect\":{\"minutes\":0,\"seconds\":0}}}"
                        |> ExamCollection.fromJson
                    )
        ]
