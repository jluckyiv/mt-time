module ExamCollection exposing
    ( ExamCollection
    , decoder
    , encode
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
import Json.Decode as Decode exposing (Decoder, Error)
import Json.Encode as Encode exposing (Value, dict)
import WitnessExam as Exam exposing (KeyedExam, WitnessExam)


type alias ExamCollection =
    Dict String WitnessExam


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
        |> List.map (\( _, exam ) -> Exam.cross exam |> Exam.toDuration)
        |> List.foldl Duration.add Duration.zero


totalCombinedDirect : ExamCollection -> Duration
totalCombinedDirect collection =
    Duration.add (totalDirect collection) (totalRedirect collection)


totalDirect : ExamCollection -> Duration
totalDirect collection =
    toList collection
        |> List.map (\( _, exam ) -> Exam.direct exam |> Exam.toDuration)
        |> List.foldl Duration.add Duration.zero


totalRedirect : ExamCollection -> Duration
totalRedirect collection =
    toList collection
        |> List.map (\( _, exam ) -> Exam.redirect exam |> Exam.toDuration)
        |> List.foldl Duration.add Duration.zero


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


updateCollection : String -> WitnessExam -> ExamCollection -> ExamCollection
updateCollection key exam collection =
    Dict.update key (\_ -> Just exam) collection


encode : ExamCollection -> Value
encode collection =
    dict identity Exam.encode collection


toJson : ExamCollection -> String
toJson collection =
    Encode.encode 0 (encode collection)


decoder : Decoder ExamCollection
decoder =
    Decode.dict Exam.examDecoder


fromJson : String -> Result Error ExamCollection
fromJson =
    Decode.decodeString decoder


fromValue : Value -> Result Error ExamCollection
fromValue =
    Decode.decodeValue decoder
