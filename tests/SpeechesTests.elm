module SpeechesTests exposing (all)

import Duration exposing (..)
import Expect
import Minutes
import Speeches exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Speeches"
        [ test "new has zero opening" <|
            \_ ->
                let
                    speeches =
                        Speeches.new
                in
                Expect.equal (Speeches.opening speeches) Duration.zero
        , test "new has zero closing" <|
            \_ ->
                let
                    speeches =
                        Speeches.new
                in
                Expect.equal (Speeches.closing speeches) Duration.zero
        , test "new has nine minutes remaining for  closing" <|
            \_ ->
                let
                    speeches =
                        Speeches.new

                    minutes =
                        Minutes.fromInt 9
                in
                Expect.equal (Just <| Duration.fromMinutes minutes) (Speeches.timeRemaining speeches)
        , test "updateOpening" <|
            \_ ->
                let
                    speeches =
                        Speeches.new
                            |> Speeches.updateOpening (Duration.create 2 13)
                in
                Expect.equal (Just <| Duration.create 6 47) (Speeches.timeRemaining speeches)
        ]
