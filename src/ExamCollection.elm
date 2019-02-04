module ExamCollection exposing
    ( ExamCollection
    , collectionDecoder
    , encodeCollection
    , fromJson
    , fromValue
    , new
    , toJson
    , toList
    , totalCombinedDirect
    , totalCross
    , updateCross
    , updateCrossWithString
    , updateDirect
    , updateDirectWithString
    , updateRedirect
    , updateRedirectWithString
    )

import Dict exposing (Dict)
import Duration exposing (..)
import Exam exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, dict, encode)


type alias ExamCollection =
    Dict String Exam


toList : ExamCollection -> List KeyedExam
toList collection =
    Dict.toList collection


new : List String -> ExamCollection
new list =
    list
        |> List.map (\key -> ( key, Exam.empty ))
        |> Dict.fromList


totalCross : ExamCollection -> Duration
totalCross collection =
    toList collection
        |> List.map (\( _, exam ) -> cross exam)
        |> List.foldl Duration.add (Duration 0 0)


totalCombinedDirect : ExamCollection -> Duration
totalCombinedDirect collection =
    Duration.add (totalDirect collection) (totalRedirect collection)


totalDirect : ExamCollection -> Duration
totalDirect collection =
    toList collection
        |> List.map (\( _, exam ) -> direct exam)
        |> List.foldl Duration.add (Duration 0 0)


totalRedirect : ExamCollection -> Duration
totalRedirect collection =
    toList collection
        |> List.map (\( _, exam ) -> redirect exam)
        |> List.foldl Duration.add (Duration 0 0)


updateDirect : Duration -> KeyedExam -> ExamCollection -> ExamCollection
updateDirect duration ( key, exam ) collection =
    let
        exam_ =
            Exam.updateDirect duration exam
    in
    updateCollection key exam_ collection


updateDirectWithString : String -> KeyedExam -> ExamCollection -> ExamCollection
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


updateRedirect : Duration -> KeyedExam -> ExamCollection -> ExamCollection
updateRedirect duration ( key, exam ) collection =
    let
        exam_ =
            Exam.updateRedirect duration exam
    in
    updateCollection key exam_ collection


updateRedirectWithString : String -> KeyedExam -> ExamCollection -> ExamCollection
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


updateCross : Duration -> KeyedExam -> ExamCollection -> ExamCollection
updateCross duration ( key, exam ) collection =
    let
        exam_ =
            Exam.updateCross duration exam
    in
    updateCollection key exam_ collection


updateCrossWithString : String -> KeyedExam -> ExamCollection -> ExamCollection
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


updateCollection : String -> Exam -> ExamCollection -> ExamCollection
updateCollection key exam collection =
    Dict.update key (\_ -> Just exam) collection


encodeCollection : ExamCollection -> Value
encodeCollection collection =
    dict identity Exam.encodeExam collection


toJson : ExamCollection -> String
toJson collection =
    encode 0 (encodeCollection collection)


collectionDecoder : Decoder ExamCollection
collectionDecoder =
    Decode.dict Exam.examDecoder


fromJson =
    Decode.decodeString collectionDecoder


fromValue =
    Decode.decodeValue collectionDecoder
