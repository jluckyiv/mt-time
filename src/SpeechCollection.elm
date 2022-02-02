module SpeechCollection exposing
    ( SpeechCollection
    , decoder
    , encode
    , new
    , updateDefenseClosing
    , updateDefenseOpening
    , updateProsecutionClosing
    , updateProsecutionOpening
    )

import Duration
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Speeches exposing (Speeches)


type alias SpeechCollection =
    { prosecution : Speeches
    , defense : Speeches
    }


new : SpeechCollection
new =
    { prosecution = Speeches.new
    , defense = Speeches.new
    }


updateProsecutionOpening : String -> SpeechCollection -> SpeechCollection
updateProsecutionOpening string speechCollection =
    let
        duration =
            string
                |> Duration.fromString
                |> Result.withDefault Duration.zero

        prosecution =
            Speeches.updateOpening duration speechCollection.prosecution
    in
    { speechCollection | prosecution = prosecution }


updateProsecutionClosing : String -> SpeechCollection -> SpeechCollection
updateProsecutionClosing string speechCollection =
    let
        duration =
            string
                |> Duration.fromString
                |> Result.withDefault Duration.zero

        prosecution =
            Speeches.updateClosing duration speechCollection.prosecution
    in
    { speechCollection | prosecution = prosecution }


updateDefenseOpening : String -> SpeechCollection -> SpeechCollection
updateDefenseOpening string speechCollection =
    let
        duration =
            string
                |> Duration.fromString
                |> Result.withDefault Duration.zero

        defense =
            Speeches.updateOpening duration speechCollection.defense
    in
    { speechCollection | defense = defense }


updateDefenseClosing : String -> SpeechCollection -> SpeechCollection
updateDefenseClosing string speechCollection =
    let
        duration =
            string
                |> Duration.fromString
                |> Result.withDefault Duration.zero

        defense =
            Speeches.updateClosing duration speechCollection.defense
    in
    { speechCollection | defense = defense }


decoder : Decoder SpeechCollection
decoder =
    Decode.succeed SpeechCollection
        |> Decode.required "prosecution" Speeches.decoder
        |> Decode.required "defense" Speeches.decoder


encode : SpeechCollection -> Value
encode speechCollection =
    Encode.object
        [ ( "prosecution", Speeches.encode speechCollection.prosecution )
        , ( "defense", Speeches.encode speechCollection.defense )
        ]
