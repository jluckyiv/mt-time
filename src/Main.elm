module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Duration exposing (Duration, fromString)
import Html exposing (Html, div, h1, h2, img, input, p, span, table, tbody, td, text, tfoot, th, thead, tr)
import Html.Attributes exposing (class, pattern, placeholder, src, type_, value)
import Html.Events exposing (onInput)
import WitnessExam exposing (..)
import WitnessExamCollection as Collection exposing (..)



---- MODEL ----


type alias UpdateCollectionMsg =
    KeyedWitnessExam -> String -> Msg


type alias Model =
    { prosecution : WitnessExamCollection
    , defense : WitnessExamCollection
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
    | UpdateProsecutionDirect KeyedWitnessExam String
    | UpdateProsecutionCross KeyedWitnessExam String
    | UpdateProsecutionRedirect KeyedWitnessExam String
    | UpdateDefenseDirect KeyedWitnessExam String
    | UpdateDefenseCross KeyedWitnessExam String
    | UpdateDefenseRedirect KeyedWitnessExam String


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
                    Collection.updateDirectWithString string keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateProsecutionCross keyedExam string ->
            ( { model
                | prosecution =
                    Collection.updateCrossWithString string keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateProsecutionRedirect keyedExam string ->
            ( { model
                | prosecution = Collection.updateRedirectWithString string keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateDefenseDirect keyedExam string ->
            ( { model
                | defense = Collection.updateDirectWithString string keyedExam model.defense
              }
            , Cmd.none
            )

        UpdateDefenseCross keyedExam string ->
            ( { model
                | defense = Collection.updateCrossWithString string keyedExam model.defense
              }
            , Cmd.none
            )

        UpdateDefenseRedirect keyedExam string ->
            ( { model
                | defense = Collection.updateRedirectWithString string keyedExam model.defense
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
                , viewProsecutionExaminations model
                , viewRemaining "Prosecution" "direct" totalProsecutionDirect 14
                , viewRemaining "Defense" "cross" totalProsecutionCross 10
                ]
            ]
        , div [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "title is-5" ] [ text "Defense" ]
                , viewDefenseExaminations model
                , viewRemaining "Defense" "direct" totalDefenseDirect 14
                , viewRemaining "Prosecution" "cross" totalDefenseCross 10
                ]
            ]
        ]


viewDefenseExaminations : Model -> Html Msg
viewDefenseExaminations model =
    viewExaminations viewDefenseExamination model.defense


viewProsecutionExaminations : Model -> Html Msg
viewProsecutionExaminations model =
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


viewExaminationsBody : (KeyedWitnessExam -> Html Msg) -> WitnessExamCollection -> Html Msg
viewExaminationsBody rowFunction collection =
    tbody [] (List.map rowFunction <| Collection.toList collection)


viewProsecutionExamination : KeyedWitnessExam -> Html Msg
viewProsecutionExamination keyedExam =
    viewExamination keyedExam UpdateProsecutionDirect UpdateProsecutionCross UpdateProsecutionRedirect


viewDefenseExamination : KeyedWitnessExam -> Html Msg
viewDefenseExamination keyedExam =
    viewExamination keyedExam UpdateDefenseDirect UpdateDefenseCross UpdateDefenseRedirect


viewExamination : KeyedWitnessExam -> UpdateCollectionMsg -> UpdateCollectionMsg -> UpdateCollectionMsg -> Html Msg
viewExamination ( key, exam ) directMsg crossMsg redirectMsg =
    tr []
        [ td [] [ inputDuration ( key, exam ) (WitnessExam.directString exam) (directMsg ( key, exam )) ]
        , td [] [ inputDuration ( key, exam ) (WitnessExam.crossString exam) (crossMsg ( key, exam )) ]
        , td [] [ inputDuration ( key, exam ) (WitnessExam.redirectString exam) (redirectMsg ( key, exam )) ]
        ]


inputDuration : KeyedWitnessExam -> String -> (String -> Msg) -> Html Msg
inputDuration ( key, exam ) string msg =
    input
        [ type_ "tel"
        , class "input"
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


viewTotalDirect : WitnessExam -> Html Msg
viewTotalDirect exam =
    WitnessExam.totalDirect exam
        |> Duration.toString
        |> text


viewRemainingDirect : WitnessExam -> Html Msg
viewRemainingDirect exam =
    WitnessExam.totalDirect exam
        |> Duration.subtract (Duration 14 0)
        |> Duration.toString
        |> text


viewTotalCross : WitnessExam -> Html Msg
viewTotalCross exam =
    WitnessExam.cross exam
        |> Duration.toString
        |> text


viewRemainingCross : WitnessExam -> Html Msg
viewRemainingCross exam =
    WitnessExam.cross exam
        |> Duration.subtract (Duration 10 0)
        |> Duration.toString
        |> text



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
