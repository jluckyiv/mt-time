module SecondsTests exposing (all)

import Expect
import Fuzz exposing (intRange)
import Seconds exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Seconds"
        [ test "Seconds.zero" <|
            \_ -> Expect.equal (Seconds.zero |> Seconds.toInt) 0
        , fuzz (intRange 0 10000000) "Seconds.fromInt" <|
            \val -> Expect.equal (Seconds.fromInt val |> Seconds.toInt) val
        , fuzz (intRange -1000000 -1) "No negative values" <|
            \val -> Expect.equal (Seconds.fromInt val) Seconds.zero
        ]
