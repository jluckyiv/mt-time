module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Duration exposing (Duration, fromString)
import Exam exposing (..)
import ExamCollection as Collection exposing (..)
import Html exposing (Html, br, div, hr, img, input, p, span, table, tbody, td, text, tfoot, th, thead, tr)
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

        UpdateProsecutionDirect keyedExam stringValue ->
            ( { model
                | prosecution =
                    updateDirect stringValue keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateProsecutionCross keyedExam stringValue ->
            ( { model
                | prosecution =
                    updateCross stringValue keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateProsecutionRedirect keyedExam stringValue ->
            ( { model
                | prosecution =
                    updateRedirect stringValue keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateDefenseDirect keyedExam stringValue ->
            ( { model
                | defense =
                    updateDirect stringValue keyedExam model.defense
              }
            , Cmd.none
            )

        UpdateDefenseCross keyedExam stringValue ->
            ( { model
                | defense =
                    updateCross stringValue keyedExam model.defense
              }
            , Cmd.none
            )

        UpdateDefenseRedirect keyedExam stringValue ->
            ( { model
                | defense =
                    updateRedirect stringValue keyedExam model.defense
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "title is-5" ] [ text "Prosecution" ]
            , viewProsecutionExams model
            , viewRemainingDirect "Prosecution" model.prosecution
            , viewRemainingCross "Defense" model.prosecution
            ]
        , br [] []
        , hr [] []
        , div [ class "container" ]
            [ div [ class "title is-5" ] [ text "Defense" ]
            , viewDefenseExams model
            , viewRemainingDirect "Defense" model.defense
            , viewRemainingCross "Prosecution" model.defense
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
        [ th [ class "has-text-centered" ] [ text "Direct" ]
        , th [ class "has-text-centered" ] [ text "Cross" ]
        , th [ class "has-text-centered" ] [ text "Redirect" ]
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
    let
        value_ =
            case string of
                "0:00" ->
                    ""

                _ ->
                    string
    in
    input
        [ type_ "tel"
        , class "input has-text-right"
        , pattern "[0-9]*"
        , value value_
        , onInput msg
        ]
        []


viewRemainingDirect side collection =
    viewRemaining side "direct" 14 (Collection.combinedDirect collection)


viewRemainingCross side collection =
    viewRemaining side "cross" 10 (Collection.totalCross collection)


viewRemaining : String -> String -> Int -> Duration -> Html Msg
viewRemaining side examType max totalDuration =
    let
        used =
            Duration.toString totalDuration

        remaining =
            Duration.toString <|
                Duration.subtract (Duration.fromMinutes max) totalDuration
    in
    div [ class "container" ]
        [ span [ class "has-text-weight-bold" ] [ text side ]
        , span [] [ text " has used " ]
        , span [ class "has-text-weight-bold" ] [ text used ]
        , span [] [ text " on " ]
        , span [ class "has-text-weight-bold" ] [ text examType ]
        , span [] [ text " and has " ]
        , span [ class "has-text-weight-bold" ] [ text remaining ]
        , span [] [ text " remaining." ]
        ]


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
