module WitnessExamCollection exposing
    ( WitnessExamCollection
    , combinedDirect
    , new
    , toList
    , totalCross
    , totalDirect
    , updateCross
    , updateCrossWithString
    , updateDirect
    , updateDirectWithString
    , updateRedirect
    , updateRedirectWithString
    )

import Dict exposing (Dict)
import Duration exposing (..)
import WitnessExam exposing (..)


type alias WitnessExamCollection =
    Dict String WitnessExam


toList : WitnessExamCollection -> List KeyedWitnessExam
toList collection =
    Dict.toList collection


new : List String -> WitnessExamCollection
new list =
    list
        |> List.map (\key -> ( key, WitnessExam.empty ))
        |> Dict.fromList


totalCross : WitnessExamCollection -> Duration
totalCross collection =
    toList collection
        |> List.map (\( _, exam ) -> cross exam)
        |> List.foldl Duration.add (Duration 0 0)


combinedDirect : WitnessExamCollection -> Duration
combinedDirect collection =
    Duration.add (totalDirect collection) (totalRedirect collection)


totalDirect : WitnessExamCollection -> Duration
totalDirect collection =
    toList collection
        |> List.map (\( _, exam ) -> direct exam)
        |> List.foldl Duration.add (Duration 0 0)


totalRedirect : WitnessExamCollection -> Duration
totalRedirect collection =
    toList collection
        |> List.map (\( _, exam ) -> redirect exam)
        |> List.foldl Duration.add (Duration 0 0)


updateDirect : Duration -> KeyedWitnessExam -> WitnessExamCollection -> WitnessExamCollection
updateDirect duration ( key, exam ) collection =
    let
        exam_ =
            WitnessExam.updateDirect duration exam
    in
    updateCollection key exam_ collection


updateDirectWithString : String -> KeyedWitnessExam -> WitnessExamCollection -> WitnessExamCollection
updateDirectWithString string keyedExam collection =
    let
        result =
            Duration.fromString string
    in
    case result of
        Ok duration ->
            updateDirect duration keyedExam collection

        Err _ ->
            collection


updateRedirect : Duration -> KeyedWitnessExam -> WitnessExamCollection -> WitnessExamCollection
updateRedirect duration ( key, exam ) collection =
    let
        exam_ =
            WitnessExam.updateRedirect duration exam
    in
    updateCollection key exam_ collection


updateRedirectWithString : String -> KeyedWitnessExam -> WitnessExamCollection -> WitnessExamCollection
updateRedirectWithString string keyedExam collection =
    let
        result =
            Duration.fromString string
    in
    case result of
        Ok duration ->
            updateRedirect duration keyedExam collection

        Err _ ->
            collection


updateCross : Duration -> KeyedWitnessExam -> WitnessExamCollection -> WitnessExamCollection
updateCross duration ( key, exam ) collection =
    let
        exam_ =
            WitnessExam.updateCross duration exam
    in
    updateCollection key exam_ collection


updateCrossWithString : String -> KeyedWitnessExam -> WitnessExamCollection -> WitnessExamCollection
updateCrossWithString string keyedExam collection =
    let
        result =
            Duration.fromString string
    in
    case result of
        Ok duration ->
            updateCross duration keyedExam collection

        Err _ ->
            collection


updateCollection : String -> WitnessExam -> WitnessExamCollection -> WitnessExamCollection
updateCollection key exam collection =
    Dict.update key (\_ -> Just exam) collection
