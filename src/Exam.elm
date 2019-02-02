module Exam exposing
    ( KeyedExam
    , Exam
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
