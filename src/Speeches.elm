module Speeches exposing
    ( Speeches
    , closing
    , decoder
    , encode
    , new
    , opening
    , timeRemaining
    , updateClosing
    , updateOpening
    )

import Duration exposing (Duration)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Minutes


type Speeches
    = Speeches Opening Closing


type Opening
    = Opening Duration


type Closing
    = Closing Duration


new : Speeches
new =
    Speeches (Opening Duration.zero) (Closing Duration.zero)


create : Duration -> Duration -> Speeches
create opening_ closing_ =
    Speeches (Opening opening_) (Closing closing_)


opening : Speeches -> Duration
opening (Speeches (Opening openingDuration) _) =
    openingDuration


closing : Speeches -> Duration
closing (Speeches _ (Closing closingDuration)) =
    closingDuration


updateOpening : Duration -> Speeches -> Speeches
updateOpening duration (Speeches _ closing_) =
    Speeches (Opening duration) closing_


updateClosing : Duration -> Speeches -> Speeches
updateClosing duration (Speeches opening_ _) =
    Speeches opening_ (Closing duration)


totalDuration : Duration
totalDuration =
    9
        |> Minutes.fromInt
        |> Duration.fromMinutes


timeRemaining : Speeches -> Maybe Duration
timeRemaining speeches =
    Duration.subtract totalDuration (opening speeches)


decoder : Decoder Speeches
decoder =
    Decode.succeed create
        |> Decode.required "opening" Duration.decoder
        |> Decode.required "closing" Duration.decoder


encode : Speeches -> Value
encode speeches =
    Encode.object
        [ ( "opening", Duration.encode (opening speeches) )
        , ( "closing", Duration.encode (closing speeches) )
        ]
