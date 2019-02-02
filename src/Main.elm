module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Duration exposing (Duration, fromString)
import Html exposing (Html, br, div, h1, h2, img, input, p, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, pattern, placeholder, src, type_, value)
import Html.Events exposing (onInput)
import WitnessExam exposing (..)
import WitnessExamCollection exposing (..)



---- MODEL ----


type alias Model =
    { prosecution : WitnessExamCollection
    , defense : WitnessExamCollection
    }


initModel : Model
initModel =
    { prosecution =
        WitnessExamCollection.new [ "P1", "P2", "P3", "P4" ]
    , defense =
        WitnessExamCollection.new [ "P1", "P2", "P3", "P4" ]
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
                    WitnessExamCollection.updateDirectWithString string keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateProsecutionCross keyedExam string ->
            ( { model
                | prosecution =
                    WitnessExamCollection.updateCrossWithString string keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateProsecutionRedirect keyedExam string ->
            ( { model
                | prosecution = WitnessExamCollection.updateRedirectWithString string keyedExam model.prosecution
              }
            , Cmd.none
            )

        UpdateDefenseDirect keyedExam string ->
            ( { model
                | defense = WitnessExamCollection.updateDirectWithString string keyedExam model.defense
              }
            , Cmd.none
            )

        UpdateDefenseCross keyedExam string ->
            ( { model
                | defense = WitnessExamCollection.updateCrossWithString string keyedExam model.defense
              }
            , Cmd.none
            )

        UpdateDefenseRedirect keyedExam string ->
            ( { model
                | defense = WitnessExamCollection.updateRedirectWithString string keyedExam model.defense
              }
            , Cmd.none
            )


updateProsecution exam prosecution =
    prosecution



---- VIEW ----


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


viewProsecutionExamination : KeyedWitnessExam -> Html Msg
viewProsecutionExamination ( key, exam ) =
    tr []
        [ td []
            [ input
                [ type_ "tel"
                , class "input"
                , pattern "[0-9]*"
                , placeholder "direct"
                , value (WitnessExam.directString exam)
                , onInput (UpdateProsecutionDirect ( key, exam ))
                ]
                []
            ]
        , td []
            [ input
                [ type_ "tel"
                , class "input"
                , pattern "[0-9]*"
                , placeholder "cross"
                , value (WitnessExam.crossString exam)
                , onInput (UpdateProsecutionCross ( key, exam ))
                ]
                []
            ]
        , td []
            [ input
                [ type_ "tel"
                , class "input"
                , pattern "[0-9]*"
                , placeholder "redirect"
                , value (WitnessExam.redirectString exam)
                , onInput (UpdateProsecutionRedirect ( key, exam ))
                ]
                []
            ]
        ]


viewDefenseExamination : KeyedWitnessExam -> Html Msg
viewDefenseExamination ( key, exam ) =
    tr []
        [ td []
            [ input
                [ type_ "tel"
                , class "input"
                , pattern "[0-9]*"
                , placeholder "direct"
                , value (WitnessExam.directString exam)
                , onInput (UpdateDefenseDirect ( key, exam ))
                ]
                []
            ]
        , td []
            [ input
                [ type_ "tel"
                , class "input"
                , pattern "[0-9]*"
                , placeholder "cross"
                , value (WitnessExam.crossString exam)
                , onInput (UpdateDefenseCross ( key, exam ))
                ]
                []
            ]
        , td []
            [ input
                [ type_ "tel"
                , class "input"
                , pattern "[0-9]*"
                , placeholder "redirect"
                , value (WitnessExam.redirectString exam)
                , onInput (UpdateDefenseRedirect ( key, exam ))
                ]
                []
            ]
        ]


view : Model -> Html Msg
view model =
    let
        totalProsecutionDirect =
            WitnessExamCollection.combinedDirect model.prosecution

        totalProsecutionCross =
            WitnessExamCollection.totalCross model.prosecution

        totalDefenseDirect =
            WitnessExamCollection.combinedDirect model.defense

        totalDefenseCross =
            WitnessExamCollection.totalCross model.defense
    in
    div []
        [ div [] [ br [] [] ]
        , table []
            [ thead [] [ td [] [ h2 [] [ text "Prosecution" ] ] ]
            , thead []
                [ td [] [ text "Direct" ]
                , td [] [ text "Cross" ]
                , td [] [ text "Redirect" ]
                ]
            , tbody []
                (List.map viewProsecutionExamination <|
                    WitnessExamCollection.toList model.prosecution
                )
            ]
        , p []
            [ text <|
                "Prosecution has used "
                    ++ Duration.toString totalProsecutionDirect
                    ++ " on direct "
                    ++ " and has "
                    ++ (Duration.toString <|
                            Duration.subtract (Duration.fromMinutes 14) totalProsecutionDirect
                       )
                    ++ " remaining"
            ]
        , p []
            [ text <|
                "Defense has used "
                    ++ Duration.toString totalProsecutionCross
                    ++ " on cross "
                    ++ " and has "
                    ++ (Duration.toString <|
                            Duration.subtract (Duration.fromMinutes 10) totalProsecutionCross
                       )
                    ++ " remaining"
            ]
        , div [] [ br [] [] ]
        , table []
            [ thead [] [ td [] [ h2 [] [ text "Defense" ] ] ]
            , thead []
                [ td [] [ text "Direct" ]
                , td [] [ text "Cross" ]
                , td [] [ text "Redirect" ]
                ]
            , tbody []
                (List.map viewDefenseExamination <|
                    WitnessExamCollection.toList model.defense
                )
            ]
        , p []
            [ text <|
                "Defense has used "
                    ++ Duration.toString totalDefenseDirect
                    ++ " on direct "
                    ++ " and has "
                    ++ (Duration.toString <|
                            Duration.subtract (Duration.fromMinutes 14) totalDefenseDirect
                       )
                    ++ " remaining"
            ]
        , p []
            [ text <|
                "Prosecution has used "
                    ++ Duration.toString totalDefenseCross
                    ++ " on cross "
                    ++ " and has "
                    ++ (Duration.toString <|
                            Duration.subtract (Duration.fromMinutes 10) totalDefenseCross
                       )
                    ++ " remaining"
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
