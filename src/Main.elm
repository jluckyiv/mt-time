port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Duration exposing (Duration)
import ExamCollection as Collection exposing (..)
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , footer
        , header
        , i
        , input
        , li
        , p
        , section
        , span
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        , ul
        )
import Html.Attributes
    exposing
        ( attribute
        , class
        , pattern
        , style
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, Error)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, object)
import SpeechCollection exposing (SpeechCollection)
import Speeches
import Warning
import WitnessExam as Exam exposing (..)



---- MODEL ----


type TabName
    = SpeechesTab
    | ProsecutionTab
    | DefenseTab


type Party
    = Prosecution
    | Defense


type alias UpdateCollectionMsg =
    KeyedExam -> String -> Msg


type alias Model =
    { speeches : SpeechCollection
    , prosecution : ExamCollection
    , defense : ExamCollection
    , modalIsOpen : Bool
    , currentTab : TabName
    }


initModel : Model
initModel =
    { speeches = SpeechCollection.new
    , prosecution =
        Collection.new [ "P1", "P2", "P3", "P4" ]
    , defense =
        Collection.new [ "D1", "D2", "D3", "D4" ]
    , modalIsOpen = False
    , currentTab = SpeechesTab
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
        [ ( "prosecution", Collection.encode model.prosecution )
        , ( "defense", Collection.encode model.defense )
        , ( "modalIsOpen", Encode.bool model.modalIsOpen )
        , ( "speeches", SpeechCollection.encode model.speeches )
        , ( "currentTab", encodeTabName model.currentTab )
        ]


encodeTabName : TabName -> Value
encodeTabName tabName =
    case tabName of
        DefenseTab ->
            Encode.string "Defense"

        ProsecutionTab ->
            Encode.string "Prosecution"

        SpeechesTab ->
            Encode.string "Speeches"


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed Model
        |> required "speeches" SpeechCollection.decoder
        |> required "prosecution" Collection.decoder
        |> required "defense" Collection.decoder
        |> required "modalIsOpen" Decode.bool
        |> required "currentTab" tabDecoder


tabDecoder : Decoder TabName
tabDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Defense" ->
                        Decode.succeed DefenseTab

                    "Prosecution" ->
                        Decode.succeed ProsecutionTab

                    _ ->
                        Decode.succeed SpeechesTab
            )


fromJson : String -> Result Error Model
fromJson =
    Decode.decodeString modelDecoder



---- UPDATE ----


type Msg
    = NoOp
    | ClearModel
    | ToggleModal
    | ChangeCurrentTab TabName
    | UpdateProsecutionDirect KeyedExam String
    | UpdateProsecutionCross KeyedExam String
    | UpdateProsecutionRedirect KeyedExam String
    | UpdateDefenseDirect KeyedExam String
    | UpdateDefenseCross KeyedExam String
    | UpdateDefenseRedirect KeyedExam String
    | UpdateProsecutionOpening String
    | UpdateProsecutionClosing String
    | UpdateDefenseOpening String
    | UpdateDefenseClosing String


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

        ChangeCurrentTab tabName ->
            ( { model | currentTab = tabName }, Cmd.none )

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

        UpdateProsecutionOpening stringValue ->
            let
                model_ =
                    { model
                        | speeches =
                            SpeechCollection.updateProsecutionOpening stringValue model.speeches
                    }
            in
            ( model_, cacheModel model_ )

        UpdateProsecutionClosing stringValue ->
            let
                model_ =
                    { model
                        | speeches =
                            SpeechCollection.updateProsecutionClosing stringValue model.speeches
                    }
            in
            ( model_, cacheModel model_ )

        UpdateDefenseOpening stringValue ->
            let
                model_ =
                    { model
                        | speeches =
                            SpeechCollection.updateDefenseOpening stringValue model.speeches
                    }
            in
            ( model_, cacheModel model_ )

        UpdateDefenseClosing stringValue ->
            let
                model_ =
                    { model
                        | speeches =
                            SpeechCollection.updateDefenseClosing stringValue model.speeches
                    }
            in
            ( model_, cacheModel model_ )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ viewTabs model.currentTab
        , viewTab model
        , viewModal model.modalIsOpen
        ]


viewTabs : TabName -> Html Msg
viewTabs party =
    let
        ( speechClass, prosecutionClass, defenseClass ) =
            tabClasses party
    in
    div [ class "tabs is-boxed" ]
        [ ul []
            [ li [ class speechClass ]
                [ a [ onClick (ChangeCurrentTab SpeechesTab) ]
                    [ text "Speeches" ]
                ]
            , li [ class prosecutionClass ]
                [ a [ onClick (ChangeCurrentTab ProsecutionTab) ]
                    [ text "Prosecution" ]
                ]
            , li [ class defenseClass ]
                [ a [ onClick (ChangeCurrentTab DefenseTab) ]
                    [ text "Defense" ]
                ]
            , clearButton
            ]
        ]


tabClasses : TabName -> ( String, String, String )
tabClasses party =
    case party of
        ProsecutionTab ->
            ( "", "is-active", "" )

        DefenseTab ->
            ( "", "", "is-active" )

        SpeechesTab ->
            ( "is-active", "", "" )


clearButton : Html Msg
clearButton =
    li []
        [ a [ class "has-text-danger", onClick ToggleModal ]
            [ span [ class "icon is-small" ] [ i [ class "fas fa-trash-alt" ] [] ]
            , span [] [ text "Clear" ]
            ]
        ]


viewModal : Bool -> Html Msg
viewModal modalIsOpen =
    if modalIsOpen then
        div [ class "modal is-active" ]
            [ div [ class "modal-background", onClick ToggleModal ] []
            , div [ class "modal-card" ]
                [ header [ class "modal-card-head" ]
                    [ p [ class "modal-card-title" ]
                        [ text "Are you sure?" ]
                    , button [ attribute "aria-label" "close", class "delete", onClick ToggleModal ] []
                    ]
                , section [ class "modal-card-body" ]
                    [ text "This will clear the worksheet." ]
                , footer [ class "modal-card-foot", style "justify-content" "flex-end" ]
                    [ button [ class "button is-danger", onClick ClearModel ]
                        [ text "Clear" ]
                    , button [ class "button", onClick ToggleModal ]
                        [ text "Cancel" ]
                    ]
                ]
            ]

    else
        text ""


viewTab : Model -> Html Msg
viewTab model =
    case model.currentTab of
        SpeechesTab ->
            viewSpeechesWorksheet model.speeches

        ProsecutionTab ->
            viewPartyWorksheet model

        DefenseTab ->
            viewPartyWorksheet model


viewSpeechesWorksheet : SpeechCollection -> Html Msg
viewSpeechesWorksheet speeches =
    div [ class "container" ]
        [ div [ class "title is-5" ] [ text "Opening and closing" ]
        , viewSpeeches speeches
        , viewRemainingProsecutionClosing "Prosecution" (Speeches.opening speeches.prosecution)
        , viewRemainingDefenseClosing "Defense" (Speeches.opening speeches.defense)
        ]


viewRemainingProsecutionClosing : String -> Duration -> Html Msg
viewRemainingProsecutionClosing side duration =
    viewRemaining side "opening" 9 duration


viewRemainingDefenseClosing : String -> Duration -> Html Msg
viewRemainingDefenseClosing side duration =
    viewRemaining side "opening" 9 duration


viewSpeeches : SpeechCollection -> Html Msg
viewSpeeches speeches =
    table [ class "table" ]
        [ viewSpeechesHead
        , viewSpeechesBody speeches
        ]


viewSpeechesHead : Html Msg
viewSpeechesHead =
    thead []
        [ th [ class "has-text-centered" ] [ text "Party" ]
        , th [ class "has-text-centered" ] [ text "Opening" ]
        , th [ class "has-text-centered" ] [ text "Closing" ]
        ]


viewSpeechesBody : SpeechCollection -> Html Msg
viewSpeechesBody speeches =
    tbody []
        [ tr []
            [ td [] [ text "Prosecution" ]
            , td [] [ inputSpeech (Speeches.opening speeches.prosecution) UpdateProsecutionOpening ]
            , td [] [ inputSpeech (Speeches.closing speeches.prosecution) UpdateProsecutionClosing ]
            ]
        , tr []
            [ td [] [ text "Defense" ]
            , td [] [ inputSpeech (Speeches.opening speeches.defense) UpdateDefenseOpening ]
            , td [] [ inputSpeech (Speeches.closing speeches.defense) UpdateDefenseClosing ]
            ]
        ]


inputSpeech : Duration -> (String -> Msg) -> Html Msg
inputSpeech duration msg =
    let
        string =
            Duration.toString duration

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


viewPartyWorksheet : Model -> Html Msg
viewPartyWorksheet model =
    let
        party =
            if model.currentTab == ProsecutionTab then
                Prosecution

            else
                Defense

        partyString =
            partyToString party

        opponentString =
            opponentToString party

        collection_ =
            partyCollection model party
    in
    div [ class "container" ]
        [ div [ class "title is-5" ] [ text (partyString ++ " witnesses") ]
        , viewExamFunction party model
        , viewRemainingDirect partyString collection_
        , viewRemainingCross opponentString collection_
        ]


viewExaminations : (KeyedExam -> Html Msg) -> ExamCollection -> Html Msg
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


viewRemainingDirect : String -> ExamCollection -> Html Msg
viewRemainingDirect side collection =
    viewRemaining side "direct" 14 (Collection.totalCombinedDirect collection)


viewRemainingCross : String -> ExamCollection -> Html Msg
viewRemainingCross side collection =
    viewRemaining side "cross" 10 (Collection.totalCross collection)


viewRemaining : String -> String -> Int -> Duration -> Html Msg
viewRemaining side examType max totalDuration =
    let
        used =
            Duration.toString totalDuration

        maxDuration =
            Duration.create max 0

        remainingDuration =
            totalDuration
                |> Warning.remaining maxDuration

        remaining =
            Duration.toString remainingDuration

        twoMinuteWarning =
            remainingDuration
                |> Warning.twoMinutes
                |> Duration.toString

        oneMinuteWarning =
            remainingDuration
                |> Warning.oneMinute
                |> Duration.toString

        thirtySecondWarning =
            remainingDuration
                |> Warning.thirtySeconds
                |> Duration.toString
    in
    p []
        [ span [ class "has-text-weight-bold" ] [ text side ]
        , span [] [ text " has used " ]
        , span [ class "has-text-weight-bold" ] [ text used ]
        , span [] [ text " on " ]
        , span [ class "has-text-weight-bold" ] [ text examType ]
        , span [] [ text " and has " ]
        , span [ class "has-text-weight-bold" ] [ text remaining ]
        , span [] [ text " remaining. " ]
        , span [ class "has-text-black has-text-weight-bold has-background-info" ] [ text <| "\u{00A0}" ++ twoMinuteWarning ++ "\u{00A0}" ]
        , span [ class "has-text-black has-text-weight-bold has-background-primary" ] [ text <| "\u{00A0}" ++ oneMinuteWarning ++ "\u{00A0}" ]
        , span [ class "has-text-black has-text-weight-bold has-background-warning" ] [ text <| "\u{00A0}" ++ thirtySecondWarning ++ "\u{00A0}" ]
        , span [ class "has-text-white has-text-weight-bold has-background-danger" ] [ text <| "\u{00A0}" ++ remaining ++ "\u{00A0}" ]
        ]


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


updateDirect : String -> KeyedExam -> ExamCollection -> ExamCollection
updateDirect string keyedExam collection =
    Collection.updateDirectWithString string keyedExam collection


updateCross : String -> KeyedExam -> ExamCollection -> ExamCollection
updateCross string keyedExam collection =
    Collection.updateCrossWithString string keyedExam collection


updateRedirect : String -> KeyedExam -> ExamCollection -> ExamCollection
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
