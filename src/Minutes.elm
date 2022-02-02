module Minutes exposing
    ( Minutes
    , decoder
    , encode
    , fromInt
    , fromSeconds
    , fromString
    , toInt
    , toSeconds
    , toString
    , zero
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Seconds exposing (Seconds)


type Minutes
    = Minutes Int



-- CREATE


zero : Minutes
zero =
    fromInt 0


fromInt : Int -> Minutes
fromInt int =
    Minutes (max 0 int)


fromSeconds : Seconds -> Minutes
fromSeconds seconds =
    let
        mins =
            Seconds.toInt seconds // 60
    in
    fromInt mins


fromString : String -> Maybe Minutes
fromString string =
    string
        |> String.toInt
        |> Maybe.map fromInt



-- CONVERT


toInt : Minutes -> Int
toInt (Minutes int) =
    int


toSeconds : Minutes -> Seconds
toSeconds minutes =
    toInt minutes
        * 60
        |> Seconds.fromInt


toString : Minutes -> String
toString minutes =
    minutes
        |> toInt
        |> String.fromInt



-- SERIALIZE


decoder : Decoder Minutes
decoder =
    Decode.map fromInt Decode.int


encode : Minutes -> Value
encode seconds =
    seconds
        |> toInt
        |> Encode.int
