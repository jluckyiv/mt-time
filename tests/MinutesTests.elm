module MinutesTests exposing (all)

import Expect
import Fuzz exposing (intRange)
import Minutes exposing (..)
import Seconds
import Test exposing (..)


all : Test
all =
    describe "Minutes"
        [ test "Minutes.zero" <|
            \_ -> Expect.equal (Minutes.zero |> Minutes.toInt) 0
        , fuzz (intRange 0 10000000) "Minutes.fromInt" <|
            \val -> Expect.equal (Minutes.fromInt val |> Minutes.toInt) val
        , fuzz (intRange 0 10000000) "Minutes.fromSeconds" <|
            \val ->
                let
                    seconds =
                        Seconds.fromInt (val * 60)
                in
                Expect.equal (Minutes.fromSeconds seconds |> Minutes.toSeconds) seconds
        , fuzz (intRange -1000000 -1) "No negative values" <|
            \val -> Expect.equal (Minutes.fromInt val) Minutes.zero
        , fuzz (intRange -1000000 -1) "No negative values fromSeconds" <|
            \val ->
                let
                    seconds =
                        Seconds.fromInt (val * 60)
                in
                Expect.equal (Minutes.fromSeconds seconds |> Minutes.toSeconds) Seconds.zero
        ]
