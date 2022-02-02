module Seconds exposing
    ( Seconds
    , add
    , compare
    , decoder
    , encode
    , fromInt
    , fromString
    , remainder
    , subtract
    , toInt
    , toString
    , zero
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Seconds
    = Seconds Int



-- CREATE


zero : Seconds
zero =
    fromInt 0


fromInt : Int -> Seconds
fromInt int =
    Seconds (max 0 int)


fromString : String -> Maybe Seconds
fromString string =
    string
        |> String.toInt
        |> Maybe.map fromInt



-- CONVERT


toInt : Seconds -> Int
toInt (Seconds int) =
    int


toString : Seconds -> String
toString seconds =
    let
        int =
            toInt seconds
    in
    if int < 10 then
        "0" ++ String.fromInt int

    else
        String.fromInt int



-- OPERATIONS


compare : Seconds -> Seconds -> Order
compare first second =
    let
        firstValue =
            toInt first

        secondValue =
            toInt second
    in
    if firstValue == secondValue then
        EQ

    else if firstValue > secondValue then
        GT

    else
        LT


add : Seconds -> Seconds -> Seconds
add seconds1 seconds2 =
    fromInt (toInt seconds1 + toInt seconds2)


subtract : Seconds -> Seconds -> Maybe Seconds
subtract seconds1 seconds2 =
    case compare seconds1 seconds2 of
        LT ->
            Nothing

        _ ->
            (toInt seconds1 - toInt seconds2) |> fromInt |> Just


remainder : Seconds -> Seconds
remainder seconds =
    seconds
        |> toInt
        |> remainderBy 60
        |> Seconds



-- SERIALIZE


decoder : Decoder Seconds
decoder =
    Decode.map fromInt Decode.int


encode : Seconds -> Value
encode seconds =
    seconds
        |> toInt
        |> Encode.int
