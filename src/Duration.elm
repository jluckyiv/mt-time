module Duration exposing
    ( Duration
    , add
    , compare
    , create
    , decoder
    , encode
    , fromInt
    , fromJson
    , fromMinutes
    , fromSeconds
    , fromString
    , fromValue
    , minutes
    , remaining
    , seconds
    , subtract
    , toJson
    , toSeconds
    , toString
    , zero
    )

import Json.Decode as Decode exposing (Decoder, Error)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, encode, object)
import Minutes exposing (..)
import Parser exposing (..)
import Seconds exposing (..)


type Duration
    = Duration Minutes Seconds



-- CREATE


zero : Duration
zero =
    Duration Minutes.zero Seconds.zero


create : Int -> Int -> Duration
create min sec =
    Duration (Minutes.fromInt min) (Seconds.fromInt sec)


fromInt : Int -> Duration
fromInt int =
    int
        |> Seconds.fromInt
        |> fromSeconds


fromMinutes : Minutes -> Duration
fromMinutes minutes_ =
    Duration minutes_ Seconds.zero


fromSeconds : Seconds -> Duration
fromSeconds seconds_ =
    Duration (Minutes.fromSeconds seconds_) (Seconds.remainder seconds_)


fromString : String -> Result (List DeadEnd) Duration
fromString str =
    str
        |> removeColon
        |> removeLeadingZeroes
        |> parse


fromJson : String -> Result Error Duration
fromJson =
    Decode.decodeString decoder


fromValue : Value -> Result Error Duration
fromValue =
    Decode.decodeValue decoder



-- CONVERT


minutes : Duration -> Int
minutes (Duration minutes_ _) =
    Minutes.toInt minutes_


seconds : Duration -> Int
seconds (Duration _ seconds_) =
    Seconds.toInt seconds_


toString : Duration -> String
toString (Duration minutes_ seconds_) =
    Minutes.toString minutes_ ++ ":" ++ Seconds.toString seconds_


toSeconds : Duration -> Seconds
toSeconds (Duration minutes_ seconds_) =
    minutes_
        |> Minutes.toInt
        |> (*) 60
        |> (+) (Seconds.toInt seconds_)
        |> Seconds.fromInt


toJson : Duration -> String
toJson duration =
    duration
        |> encode
        |> Encode.encode 0



-- OPERATIONS


compare : Duration -> Duration -> Order
compare first second =
    let
        firstSeconds =
            toSeconds first

        secondSeconds =
            toSeconds second
    in
    Seconds.compare firstSeconds secondSeconds


add : Duration -> Duration -> Duration
add duration1 duration2 =
    let
        seconds1 =
            toSeconds duration1

        seconds2 =
            toSeconds duration2
    in
    fromSeconds (Seconds.add seconds1 seconds2)


subtract : Duration -> Duration -> Maybe Duration
subtract duration1 duration2 =
    let
        seconds1 =
            toSeconds duration1

        seconds2 =
            toSeconds duration2
    in
    Seconds.subtract seconds1 seconds2
        |> Maybe.map fromSeconds


remaining : Duration -> List Duration -> Duration
remaining minuend subtrahends =
    let
        secondsMinuend =
            minuend
                |> toSeconds
                |> Seconds.toInt

        secondsSubtrahend =
            subtrahends
                |> List.map toSeconds
                |> List.map Seconds.toInt
                |> List.sum
    in
    (secondsMinuend - secondsSubtrahend)
        |> Seconds.fromInt
        |> fromSeconds



-- SERIALIZE


decoder : Decoder Duration
decoder =
    Decode.succeed Duration
        |> required "minutes" Minutes.decoder
        |> required "seconds" Seconds.decoder


encode : Duration -> Value
encode (Duration minutes_ seconds_) =
    object
        [ ( "minutes", Minutes.encode minutes_ )
        , ( "seconds", Seconds.encode seconds_ )
        ]



-- HELPERS


parse : String -> Result (List DeadEnd) Duration
parse str =
    case String.trim str of
        "" ->
            Ok zero

        s ->
            s |> Parser.run parseDuration


removeColon : String -> String
removeColon string =
    string
        |> String.replace ":" ""


removeLeadingZeroes : String -> String
removeLeadingZeroes string =
    if String.startsWith "0" string then
        String.dropLeft 1 string
            |> removeLeadingZeroes

    else
        string


fromBase100 : Int -> Duration
fromBase100 num =
    Duration (Minutes.fromInt (num // 100)) (Seconds.fromInt (remainderBy 100 num))


parseDuration : Parser Duration
parseDuration =
    succeed fromBase100 |= int
