port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Duration exposing (Duration, fromString)
import Exam exposing (..)
import ExamCollection as Collection exposing (..)
import Html exposing (Html, br, button, div, footer, header, hr, img, input, p, section, span, table, tbody, td, text, tfoot, th, thead, tr)
import Html.Attributes exposing (attribute, class, pattern, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, encode, object)



---- MODEL ----


type Party
    = Prosecution
    | Defense


type alias UpdateCollectionMsg =
    KeyedExam -> String -> Msg


type alias Model =
    { prosecution : ExamCollection
    , defense : ExamCollection
    , modalIsOpen : Bool
    }


initModel : Model
initModel =
    { prosecution =
        Collection.new [ "P1", "P2", "P3", "P4" ]
    , defense =
        Collection.new [ "P1", "P2", "P3", "P4" ]
    , modalIsOpen = False
    }


init : String -> ( Model, Cmd Msg )
init flags =
    let
        result =
            fromJson flags
    in
    case result of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( initModel, Cmd.none )



---- INTEROP ----


cacheModel : Model -> Cmd Msg
cacheModel model =
    model
        |> encodeModel
        |> cache


port cache : Value -> Cmd msg


encodeModel : Model -> Value
encodeModel model =
    object
        [ ( "prosecution", Collection.encodeCollection model.prosecution )
        , ( "defense", Collection.encodeCollection model.defense )
        , ( "modalIsOpen", Encode.bool model.modalIsOpen )
        ]


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed Model
        |> required "prosecution" Collection.collectionDecoder
        |> required "defense" Collection.collectionDecoder
        |> required "modalIsOpen" Decode.bool


fromJson =
    Decode.decodeString modelDecoder


fromValue =
    Decode.decodeValue modelDecoder



---- UPDATE ----


type Msg
    = NoOp
    | ClearModel
    | ToggleModal
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

        ClearModel ->
            let
                model_ =
                    initModel
            in
            ( model_, cacheModel model_ )

        ToggleModal ->
            let
                model_ =
                    { model
                        | modalIsOpen = not model.modalIsOpen
                    }
            in
            ( model_, cacheModel model_ )

        UpdateProsecutionDirect keyedExam stringValue ->
            let
                model_ =
                    { model
                        | prosecution =
                            updateDirect stringValue keyedExam model.prosecution
                    }
            in
            ( model_, cacheModel model_ )

        UpdateProsecutionCross keyedExam stringValue ->
            let
                model_ =
                    { model
                        | prosecution = updateCross stringValue keyedExam model.prosecution
                    }
            in
            ( model_, cacheModel model_ )

        UpdateProsecutionRedirect keyedExam stringValue ->
            let
                model_ =
                    { model
                        | prosecution =
                            updateRedirect stringValue keyedExam model.prosecution
                    }
            in
            ( model_, cacheModel model_ )

        UpdateDefenseDirect keyedExam stringValue ->
            let
                model_ =
                    { model
                        | defense =
                            updateDirect stringValue keyedExam model.defense
                    }
            in
            ( model_, cacheModel model_ )

        UpdateDefenseCross keyedExam stringValue ->
            let
                model_ =
                    { model
                        | defense =
                            updateCross stringValue keyedExam model.defense
                    }
            in
            ( model_, cacheModel model_ )

        UpdateDefenseRedirect keyedExam stringValue ->
            let
                model_ =
                    { model
                        | defense =
                            updateRedirect stringValue keyedExam model.defense
                    }
            in
            ( model_, cacheModel model_ )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ viewPartyWorksheet model Prosecution
        , viewSeparator
        , viewPartyWorksheet model Defense
        , viewSeparator
        , clearButton
        , viewModal model.modalIsOpen
        ]


clearButton : Html Msg
clearButton =
    div [ class "container"]
        [ button [ class "button is-primary", onClick ToggleModal ] [ text "Clear" ]
        ]


viewModal : Bool -> Html Msg
viewModal modalIsOpen =
    case modalIsOpen of
        True ->
            div [ class "modal is-active" ]
                [ div [ class "modal-background", onClick ToggleModal ] []
                , div [ class "modal-card" ]
                    [ header [ class "modal-card-head" ]
                        [ p [ class "modal-card-title" ]
                            [ text "Clear" ]
                        , button [ attribute "aria-label" "close", class "delete", onClick ToggleModal ] []
                        ]
                    , section [ class "modal-card-body" ]
                        [ text "This will clear all values." ]
                    , footer [ class "modal-card-foot" ]
                        [ button [ class "button is-danger", onClick ClearModel ]
                            [ text "Clear" ]
                        , button [ class "button", onClick ToggleModal ]
                            [ text "Cancel" ]
                        ]
                    ]
                ]

        False ->
            text ""


viewSeparator : Html msg
viewSeparator =
    div []
        [ br [] []
        , hr [] []
        ]


viewPartyWorksheet : Model -> Party -> Html Msg
viewPartyWorksheet model party =
    let
        partyString =
            partyToString party

        opponentString =
            opponentToString party

        collection_ =
            partyCollection model party

        viewExamFunction_ =
            viewExamFunction party
    in
    div [ class "container" ]
        [ div [ class "title is-5" ] [ text partyString ]
        , viewExamFunction_ model
        , viewRemainingDirect partyString collection_
        , viewRemainingCross opponentString collection_
        ]


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
    viewRemaining side "direct" 14 (Collection.totalCombinedDirect collection)


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
    p []
        [ span [ class "has-text-weight-bold" ] [ text side ]
        , span [] [ text " has used " ]
        , span [ class "has-text-weight-bold" ] [ text used ]
        , span [] [ text " on " ]
        , span [ class "has-text-weight-bold" ] [ text examType ]
        , span [] [ text " and has " ]
        , span [ class "has-text-weight-bold" ] [ text remaining ]
        , span [] [ text " remaining." ]
        ]



---- HELPERS ----


partyToString : Party -> String
partyToString party_ =
    case party_ of
        Prosecution ->
            "Prosecution"

        Defense ->
            "Defense"


opponentToString : Party -> String
opponentToString party_ =
    party_ |> opponent |> partyToString


opponent : Party -> Party
opponent party_ =
    case party_ of
        Prosecution ->
            Defense

        Defense ->
            Prosecution


partyCollection : Model -> Party -> ExamCollection
partyCollection model party =
    case party of
        Prosecution ->
            model.prosecution

        Defense ->
            model.defense


viewExamFunction : Party -> (Model -> Html Msg)
viewExamFunction party =
    case party of
        Prosecution ->
            viewProsecutionExams

        Defense ->
            viewDefenseExams


viewDefenseExams : Model -> Html Msg
viewDefenseExams model =
    viewExaminations viewDefenseExamination model.defense


viewProsecutionExams : Model -> Html Msg
viewProsecutionExams model =
    viewExaminations viewProsecutionExamination model.prosecution


updateDirect string keyedExam collection =
    Collection.updateDirectWithString string keyedExam collection


updateCross string keyedExam collection =
    Collection.updateCrossWithString string keyedExam collection


updateRedirect string keyedExam collection =
    Collection.updateRedirectWithString string keyedExam collection



---- PROGRAM ----


main : Program String Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
