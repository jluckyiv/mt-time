module WitnessExam exposing
    ( KeyedWitnessExam
    , WitnessExam
    , cross
    , crossString
    , direct
    , directString
    , empty
    , redirect
    , redirectString
    , totalDirect
    , updateCross
    , updateDirect
    , updateRedirect
    )

import Duration exposing (..)


type alias Direct =
    Duration


type alias Redirect =
    Duration


type alias Cross =
    Duration


type WitnessExam
    = WitnessExam Direct Cross Redirect


type alias KeyedWitnessExam =
    ( String, WitnessExam )


empty : WitnessExam
empty =
    WitnessExam Duration.empty Duration.empty Duration.empty


direct : WitnessExam -> Duration
direct (WitnessExam direct_ _ _) =
    direct_


cross : WitnessExam -> Duration
cross (WitnessExam _ cross_ _) =
    cross_


redirect : WitnessExam -> Duration
redirect (WitnessExam _ _ redirect_) =
    redirect_


updateDirect : Duration -> WitnessExam -> WitnessExam
updateDirect duration exam =
    WitnessExam duration (cross exam) (redirect exam)


updateRedirect : Duration -> WitnessExam -> WitnessExam
updateRedirect duration exam =
    WitnessExam (direct exam) (cross exam) duration


updateCross : Duration -> WitnessExam -> WitnessExam
updateCross duration exam =
    WitnessExam (direct exam) duration (redirect exam)


directString : WitnessExam -> String
directString exam =
    exam
        |> direct
        |> Duration.toString


redirectString : WitnessExam -> String
redirectString exam =
    exam
        |> redirect
        |> Duration.toString


crossString : WitnessExam -> String
crossString exam =
    exam
        |> cross
        |> Duration.toString


totalDirect : WitnessExam -> Duration
totalDirect exam =
    Duration.add (direct exam) (redirect exam)
