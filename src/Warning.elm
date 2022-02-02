module Warning exposing (oneMinute, remaining, stop, thirtySeconds, twoMinutes)

import Duration exposing (Duration)


remaining : Duration -> Duration -> Duration
remaining total used =
    used
        |> Duration.subtract total
        |> Maybe.withDefault Duration.zero


stop : Duration -> Duration
stop duration =
    duration


thirtySeconds : Duration -> Duration
thirtySeconds duration =
    Duration.create 0 30
        |> Duration.subtract duration
        |> Maybe.withDefault Duration.zero


oneMinute : Duration -> Duration
oneMinute duration =
    Duration.create 1 0
        |> Duration.subtract duration
        |> Maybe.withDefault Duration.zero


twoMinutes : Duration -> Duration
twoMinutes duration =
    Duration.create 2 0
        |> Duration.subtract duration
        |> Maybe.withDefault Duration.zero
