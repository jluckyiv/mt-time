module Duration exposing
    ( Duration
    , add
    , empty
    , fromSeconds
    , fromMinutes
    , fromString
    , remaining
    , subtract
    , toSeconds
    , toString
    )

import Parser exposing (..)


type alias Duration =
    { minutes : Minutes, seconds : Seconds }


type alias Minutes =
    Int


type alias Seconds =
    Int


fromString : String -> Result (List DeadEnd) Duration
fromString str =
    str
        |> removeColon
        |> removeLeadingZeroes
        |> parse


parse : String -> Result (List DeadEnd) Duration
parse str =
    case String.trim str of
        "" ->
            Ok empty

        s ->
            s |> Parser.run parseDuration


removeColon string =
    string
        |> String.replace ":" ""


removeLeadingZeroes string =
    if String.startsWith "0" string then
        String.dropLeft 1 string
            |> removeLeadingZeroes

    else
        string


toString : Duration -> String
toString duration =
    let
        minutes =
            duration.minutes

        seconds =
            duration.seconds
    in
    String.fromInt duration.minutes ++ ":" ++ formatSeconds duration.seconds


formatSeconds : Int -> String
formatSeconds int =
    if int < 10 then
        "0" ++ String.fromInt int

    else
        String.fromInt int


formatMinutes : Int -> String
formatMinutes int =
    String.fromInt int


fromMinutes : Minutes -> Duration
fromMinutes minutes =
    Duration minutes 0


fromSeconds : Seconds -> Duration
fromSeconds seconds =
    Duration (seconds // 60) (remainderBy 60 seconds)


toSeconds : Duration -> Seconds
toSeconds duration =
    duration.minutes * 60 + duration.seconds


add : Duration -> Duration -> Duration
add duration1 duration2 =
    fromSeconds (toSeconds duration1 + toSeconds duration2)


subtract : Duration -> Duration -> Duration
subtract duration1 duration2 =
    fromSeconds (toSeconds duration1 - toSeconds duration2)


remaining : Duration -> List Duration -> Duration
remaining minuend subtrahends =
    let
        secondsMinuend =
            toSeconds minuend

        secondsSubtrahend =
            subtrahends
                |> List.map toSeconds
                |> List.sum
    in
    fromSeconds (secondsMinuend - secondsSubtrahend)


empty : Duration
empty =
    Duration 0 0


fromBase100 : Int -> Duration
fromBase100 num =
    Duration (num // 100) (remainderBy 100 num)


parseDuration : Parser Duration
parseDuration =
    succeed fromBase100 |= int
