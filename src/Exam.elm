module Exam exposing
    ( Exam
    , KeyedExam
    , cross
    , crossString
    , direct
    , directString
    , empty
    , encodeExam
    , examDecoder
    , fromJson
    , fromValue
    , redirect
    , redirectString
    , toJson
    , totalDirect
    , updateCross
    , updateDirect
    , updateRedirect
    )

import Duration exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, encode, object)


type alias Direct =
    Duration


type alias Redirect =
    Duration


type alias Cross =
    Duration


type Exam
    = Exam Direct Cross Redirect


type alias KeyedExam =
    ( String, Exam )


empty : Exam
empty =
    Exam Duration.empty Duration.empty Duration.empty


direct : Exam -> Duration
direct (Exam direct_ _ _) =
    direct_


cross : Exam -> Duration
cross (Exam _ cross_ _) =
    cross_


redirect : Exam -> Duration
redirect (Exam _ _ redirect_) =
    redirect_


updateDirect : Duration -> Exam -> Exam
updateDirect duration exam =
    Exam duration (cross exam) (redirect exam)


updateRedirect : Duration -> Exam -> Exam
updateRedirect duration exam =
    Exam (direct exam) (cross exam) duration


updateCross : Duration -> Exam -> Exam
updateCross duration exam =
    Exam (direct exam) duration (redirect exam)


directString : Exam -> String
directString exam =
    exam
        |> direct
        |> Duration.toString


redirectString : Exam -> String
redirectString exam =
    exam
        |> redirect
        |> Duration.toString


crossString : Exam -> String
crossString exam =
    exam
        |> cross
        |> Duration.toString


totalDirect : Exam -> Duration
totalDirect exam =
    Duration.add (direct exam) (redirect exam)


encodeExam : Exam -> Value
encodeExam exam =
    object
        [ ( "direct", Duration.encodeDuration (direct exam) )
        , ( "cross", Duration.encodeDuration (cross exam) )
        , ( "redirect", Duration.encodeDuration (redirect exam) )
        ]


toJson : Exam -> String
toJson exam =
    encode 0 (encodeExam exam)


examDecoder : Decoder Exam
examDecoder =
    Decode.succeed Exam
        |> required "direct" Duration.durationDecoder
        |> required "cross" Duration.durationDecoder
        |> required "redirect" Duration.durationDecoder


fromJson =
    Decode.decodeString examDecoder


fromValue =
    Decode.decodeValue examDecoder
