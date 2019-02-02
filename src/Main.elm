module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Duration exposing (Duration, fromString)
import Exam exposing (..)
import ExamCollection as Collection exposing (..)
import Html exposing (Html, div, h1, h2, img, input, p, span, table, tbody, td, text, tfoot, th, thead, tr)
import Html.Attributes exposing (class, pattern, placeholder, src, type_, value)
import Html.Events exposing (onInput)



---- MODEL ----


type alias UpdateCollectionMsg =
    KeyedExam -> String -> Msg


type alias Model =
    { prosecution : ExamCollection
    , defense : ExamCollection
    }


initModel : Model
initModel =
    { prosecution =
        Collection.new [ "P1", "P2", "P3", "P4" ]
    , defense =
        Collection.new [ "P1", "P2", "P3", "P4" ]
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateUI String
    | UpdateProsecutionDirect KeyedExam String
    | UpdateProsecutionCross KeyedExam String
    | UpdateProsecutionRedirect KeyedExam String
    | UpdateDefenseDirect KeyedExam String
    | UpdateDefenseCross KeyedExam String
    | UpdateDefenseRedirect KeyedExam String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateUI string ->
            ( model, Cmd.none )

        UpdateProsecutionDirect keyedExam string ->
            ( { model
                | prosecution =
                    updateDirect string keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateProsecutionCross keyedExam string ->
            ( { model
                | prosecution =
                    updateCross string keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateProsecutionRedirect keyedExam string ->
            ( { model
                | prosecution =
                    updateRedirect string keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateDefenseDirect keyedExam string ->
            ( { model
                | defense =
                    updateDirect string keyedExam model.defense
              }
            , Cmd.none
            )

        UpdateDefenseCross keyedExam string ->
            ( { model
                | defense =
                    updateCross string keyedExam model.defense
              }
            , Cmd.none
            )

        UpdateDefenseRedirect keyedExam string ->
            ( { model
                | defense =
                    updateRedirect string keyedExam model.defense
              }
            , Cmd.none
            )


updateProsecution exam prosecution =
    prosecution



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        totalProsecutionDirect =
            Collection.combinedDirect model.prosecution

        totalProsecutionCross =
            Collection.totalCross model.prosecution

        totalDefenseDirect =
            Collection.combinedDirect model.defense

        totalDefenseCross =
            Collection.totalCross model.defense
    in
    div []
        [ div [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "title is-5" ] [ text "Prosecution" ]
                , viewProsecutionExams model
                , viewRemaining "Prosecution" "direct" totalProsecutionDirect 14
                , viewRemaining "Defense" "cross" totalProsecutionCross 10
                ]
            ]
        , div [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "title is-5" ] [ text "Defense" ]
                , viewDefenseExams model
                , viewRemaining "Defense" "direct" totalDefenseDirect 14
                , viewRemaining "Prosecution" "cross" totalDefenseCross 10
                ]
            ]
        ]


viewDefenseExams : Model -> Html Msg
viewDefenseExams model =
    viewExaminations viewDefenseExamination model.defense


viewProsecutionExams : Model -> Html Msg
viewProsecutionExams model =
    viewExaminations viewProsecutionExamination model.prosecution


viewExaminations viewFunction collection =
    table [ class "table" ]
        [ viewExaminationsHead
        , viewExaminationsBody viewFunction collection
        ]


viewExaminationsHead : Html Msg
viewExaminationsHead =
    thead []
        [ th [] [ text "Direct" ]
        , th [] [ text "Cross" ]
        , th [] [ text "Redirect" ]
        ]


viewExaminationsBody : (KeyedExam -> Html Msg) -> ExamCollection -> Html Msg
viewExaminationsBody rowFunction collection =
    tbody [] (List.map rowFunction <| Collection.toList collection)


viewProsecutionExamination : KeyedExam -> Html Msg
viewProsecutionExamination keyedExam =
    viewExamination keyedExam ( UpdateProsecutionDirect, UpdateProsecutionCross, UpdateProsecutionRedirect )


viewDefenseExamination : KeyedExam -> Html Msg
viewDefenseExamination keyedExam =
    viewExamination keyedExam ( UpdateDefenseDirect, UpdateDefenseCross, UpdateDefenseRedirect )


viewExamination : KeyedExam -> ( UpdateCollectionMsg, UpdateCollectionMsg, UpdateCollectionMsg ) -> Html Msg
viewExamination ( key, exam ) ( directMsg, crossMsg, redirectMsg ) =
    tr []
        [ td [] [ inputDuration ( key, exam ) (Exam.directString exam) (directMsg ( key, exam )) ]
        , td [] [ inputDuration ( key, exam ) (Exam.crossString exam) (crossMsg ( key, exam )) ]
        , td [] [ inputDuration ( key, exam ) (Exam.redirectString exam) (redirectMsg ( key, exam )) ]
        ]


inputDuration : KeyedExam -> String -> (String -> Msg) -> Html Msg
inputDuration ( key, exam ) string msg =
    input
        [ type_ "tel"
        , class "input has-text-right"
        , pattern "[0-9]*"
        , value string
        , onInput msg
        ]
        []


viewRemaining : String -> String -> Duration -> Int -> Html Msg
viewRemaining side examType total max =
    p []
        [ text <|
            side
                ++ " has used "
                ++ Duration.toString total
                ++ " on "
                ++ examType
                ++ " and has "
                ++ (Duration.toString <|
                        Duration.subtract (Duration.fromMinutes max) total
                   )
                ++ " remaining."
        ]


viewTotalDirect : Exam -> Html Msg
viewTotalDirect exam =
    Exam.totalDirect exam
        |> Duration.toString
        |> text


viewRemainingDirect : Exam -> Html Msg
viewRemainingDirect exam =
    Exam.totalDirect exam
        |> Duration.subtract (Duration 14 0)
        |> Duration.toString
        |> text


viewTotalCross : Exam -> Html Msg
viewTotalCross exam =
    Exam.cross exam
        |> Duration.toString
        |> text


viewRemainingCross : Exam -> Html Msg
viewRemainingCross exam =
    Exam.cross exam
        |> Duration.subtract (Duration 10 0)
        |> Duration.toString
        |> text


updateDirect string keyedExam collection =
    Collection.updateDirectWithString string keyedExam collection


updateCross string keyedExam collection =
    Collection.updateCrossWithString string keyedExam collection


updateRedirect string keyedExam collection =
    Collection.updateRedirectWithString string keyedExam collection



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
