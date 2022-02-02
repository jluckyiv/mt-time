module WarningTests exposing (all)

import Duration exposing (..)
import Expect
import Test exposing (..)
import Warning exposing (..)


all : Test
all =
    describe "Warning"
        [ test "two minutes" <|
            \_ ->
                let
                    duration =
                        Duration.create 2 13
                in
                Expect.equal (Warning.twoMinutes duration) (Duration.create 0 13)
        , test "under two minutes" <|
            \_ ->
                let
                    duration =
                        Duration.create 0 13
                in
                Expect.equal (Warning.twoMinutes duration) Duration.zero
        , test "one minute" <|
            \_ ->
                let
                    duration =
                        Duration.create 2 13
                in
                Expect.equal (Warning.oneMinute duration) (Duration.create 1 13)
        , test "under one minute" <|
            \_ ->
                let
                    duration =
                        Duration.create 0 13
                in
                Expect.equal (Warning.oneMinute duration) Duration.zero
        , test "thirty seconds" <|
            \_ ->
                let
                    duration =
                        Duration.create 2 13
                in
                Expect.equal (Warning.thirtySeconds duration) (Duration.create 1 43)
        , test "under thirty seconds" <|
            \_ ->
                let
                    duration =
                        Duration.create 0 13
                in
                Expect.equal (Warning.thirtySeconds duration) Duration.zero
        , test "stop" <|
            \_ ->
                let
                    duration =
                        Duration.create 2 13
                in
                Expect.equal (Warning.stop duration) (Duration.create 2 13)
        , test "time remaining" <|
            \_ ->
                let
                    used =
                        Duration.create 2 13

                    total =
                        Duration.create 9 0
                in
                Expect.equal (Warning.remaining total used) (Duration.create 6 47)
        , test "no time remaining" <|
            \_ ->
                let
                    used =
                        Duration.create 9 0

                    total =
                        Duration.create 9 0
                in
                Expect.equal (Warning.remaining total used) Duration.zero
        , test "negative time remaining" <|
            \_ ->
                let
                    used =
                        Duration.create 10 0

                    total =
                        Duration.create 9 0
                in
                Expect.equal (Warning.remaining total used) Duration.zero
        ]
