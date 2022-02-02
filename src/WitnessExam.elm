module WitnessExam exposing
    ( KeyedExam
    , WitnessExam
    , create
    , cross
    , crossString
    , direct
    , directString
    , empty
    , encode
    , examDecoder
    , fromJson
    , fromValue
    , redirect
    , redirectString
    , toDuration
    , toJson
    , totalDirect
    , updateCross
    , updateDirect
    , updateRedirect
    )

import Duration exposing (..)
import Json.Decode as Decode exposing (Decoder, Error)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, encode, object)


type Direct
    = Direct


type Redirect
    = Redirect


type Cross
    = Cross


toDuration : Exam a -> Duration
toDuration (Exam duration) =
    duration


toDirect : Duration -> Exam Direct
toDirect duration =
    Exam duration


toRedirect : Duration -> Exam Redirect
toRedirect duration =
    Exam duration


toCross : Duration -> Exam Cross
toCross duration =
    Exam duration


type Exam a
    = Exam Duration


type WitnessExam
    = WitnessExam (Exam Direct) (Exam Cross) (Exam Redirect)


type alias KeyedExam =
    ( String, WitnessExam )


empty : WitnessExam
empty =
    WitnessExam (toDirect Duration.zero) (toCross Duration.zero) (toRedirect Duration.zero)


create : Duration -> Duration -> Duration -> WitnessExam
create direct_ cross_ redirect_ =
    WitnessExam (toDirect direct_) (toCross cross_) (toRedirect redirect_)


direct : WitnessExam -> Exam Direct
direct (WitnessExam direct_ _ _) =
    direct_


cross : WitnessExam -> Exam Cross
cross (WitnessExam _ cross_ _) =
    cross_


redirect : WitnessExam -> Exam Redirect
redirect (WitnessExam _ _ redirect_) =
    redirect_


updateDirect : Duration -> WitnessExam -> WitnessExam
updateDirect duration exam =
    WitnessExam (toDirect duration) (cross exam) (redirect exam)


updateRedirect : Duration -> WitnessExam -> WitnessExam
updateRedirect duration exam =
    WitnessExam (direct exam) (cross exam) (toRedirect duration)


updateCross : Duration -> WitnessExam -> WitnessExam
updateCross duration exam =
    WitnessExam (direct exam) (toCross duration) (redirect exam)


directString : WitnessExam -> String
directString witnessExam =
    witnessExam
        |> direct
        |> toDuration
        |> Duration.toString


redirectString : WitnessExam -> String
redirectString witnessExam =
    witnessExam
        |> redirect
        |> toDuration
        |> Duration.toString


crossString : WitnessExam -> String
crossString witnessExam =
    witnessExam
        |> cross
        |> toDuration
        |> Duration.toString


totalDirect : WitnessExam -> Duration
totalDirect witnessExam =
    addDirectAndRedirect (direct witnessExam) (redirect witnessExam)


addDirectAndRedirect : Exam Direct -> Exam Redirect -> Duration
addDirectAndRedirect direct_ redirect_ =
    Duration.add (direct_ |> toDuration) (redirect_ |> toDuration)


encode : WitnessExam -> Value
encode witnessExam =
    let
        direct_ =
            witnessExam |> direct |> toDuration

        cross_ =
            witnessExam |> cross |> toDuration

        redirect_ =
            witnessExam |> redirect |> toDuration
    in
    object
        [ ( "direct", Duration.encode direct_ )
        , ( "cross", Duration.encode cross_ )
        , ( "redirect", Duration.encode redirect_ )
        ]


toJson : WitnessExam -> String
toJson exam =
    Encode.encode 0 (encode exam)


examDecoder : Decoder WitnessExam
examDecoder =
    Decode.succeed create
        |> required "direct" Duration.decoder
        |> required "cross" Duration.decoder
        |> required "redirect" Duration.decoder


fromJson : String -> Result Error WitnessExam
fromJson =
    Decode.decodeString examDecoder


fromValue : Value -> Result Error WitnessExam
fromValue =
    Decode.decodeValue examDecoder
