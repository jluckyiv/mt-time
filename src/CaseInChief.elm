module CaseInChief exposing
    ( CaseInChief
    , caseInChiefDecoder
    , encodeCaseInChief
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
import Duration exposing (Duration, add)
import Exam exposing (Exam, WitnessExam, direct, cross, redirect)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value, dict, encode)


type alias CaseInChief =
    Dict String Exam


toList : CaseInChief -> List WitnessExam
toList caseInChief =
    Dict.toList caseInChief


new : List String -> CaseInChief
new list =
    list
        |> List.map (\key -> ( key, Exam.empty ))
        |> Dict.fromList


totalCross : CaseInChief -> Duration
totalCross caseInChief =
    toList caseInChief
        |> List.map (\( _, exam ) -> cross exam)
        |> List.foldl Duration.add (Duration 0 0)


totalCombinedDirect : CaseInChief -> Duration
totalCombinedDirect caseInChief =
    Duration.add (totalDirect caseInChief) (totalRedirect caseInChief)


totalDirect : CaseInChief -> Duration
totalDirect caseInChief =
    toList caseInChief
        |> List.map (\( _, exam ) -> direct exam)
        |> List.foldl Duration.add (Duration 0 0)


totalRedirect : CaseInChief -> Duration
totalRedirect caseInChief =
    toList caseInChief
        |> List.map (\( _, exam ) -> redirect exam)
        |> List.foldl Duration.add (Duration 0 0)


updateDirect : Duration -> WitnessExam -> CaseInChief -> CaseInChief
updateDirect duration ( key, exam ) caseInChief =
    let
        exam_ =
            Exam.updateDirect duration exam
    in
    updateCaseInChief key exam_ caseInChief


updateDirectWithString : String -> WitnessExam -> CaseInChief -> CaseInChief
updateDirectWithString string witnessExam caseInChief =
    let
        result =
            Duration.fromString string
    in
    case result of
        Ok duration ->
            updateDirect duration witnessExam caseInChief

        Err _ ->
            caseInChief


updateRedirect : Duration -> WitnessExam -> CaseInChief -> CaseInChief
updateRedirect duration ( key, exam ) caseInChief =
    let
        exam_ =
            Exam.updateRedirect duration exam
    in
    updateCaseInChief key exam_ caseInChief


updateRedirectWithString : String -> WitnessExam -> CaseInChief -> CaseInChief
updateRedirectWithString string witnessExam caseInChief =
    let
        result =
            Duration.fromString string
    in
    case result of
        Ok duration ->
            updateRedirect duration witnessExam caseInChief

        Err _ ->
            caseInChief


updateCross : Duration -> WitnessExam -> CaseInChief -> CaseInChief
updateCross duration ( key, exam ) caseInChief =
    let
        exam_ =
            Exam.updateCross duration exam
    in
    updateCaseInChief key exam_ caseInChief


updateCrossWithString : String -> WitnessExam -> CaseInChief -> CaseInChief
updateCrossWithString string witnessExam caseInChief =
    let
        result =
            Duration.fromString string
    in
    case result of
        Ok duration ->
            updateCross duration witnessExam caseInChief

        Err _ ->
            caseInChief


updateCaseInChief : String -> Exam -> CaseInChief -> CaseInChief
updateCaseInChief key exam caseInChief =
    Dict.update key (\_ -> Just exam) caseInChief


encodeCaseInChief : CaseInChief -> Value
encodeCaseInChief caseInChief =
    dict identity Exam.encodeExam caseInChief


toJson : CaseInChief -> String
toJson caseInChief =
    encode 0 (encodeCaseInChief caseInChief)


caseInChiefDecoder : Decoder CaseInChief
caseInChiefDecoder =
    Decode.dict Exam.examDecoder


fromJson : String -> Result Decode.Error CaseInChief
fromJson =
    Decode.decodeString caseInChiefDecoder


fromValue : Value -> Result Decode.Error CaseInChief
fromValue =
    Decode.decodeValue caseInChiefDecoder
