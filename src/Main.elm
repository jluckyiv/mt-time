port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import CaseInChief exposing (CaseInChief)
import Duration exposing (Duration)
import Exam exposing (WitnessExam)
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
        , classList
        , href
        , pattern
        , style
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode exposing (Value, object)
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)



---- PROGRAM ----


main : Program (Maybe Value) Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



---- MODEL ----


init : Maybe Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeFlags url key =
    case maybeFlags of
        Nothing ->
            ( initModel (parseParty url) key, requestData Nothing )

        Just value ->
            ( fromValue key value, Cmd.none )


initModel : Party -> Nav.Key -> Model
initModel party key =
    { key = key
    , party = party
    , prosecution =
        CaseInChief.new [ "P1", "P2", "P3", "P4" ]
    , defense =
        CaseInChief.new [ "D1", "D2", "D3", "D4" ]
    , clearDialogIsActive = False
    }


type alias Model =
    
    { key : Nav.Key
    , prosecution : CaseInChief
    , defense : CaseInChief
    , clearDialogIsActive : Bool
    , party : Party
    }


type Party
    = Prosecution
    | Defense


type ExamType
    = Direct
    | Cross
    | Redirect


parseParty : Url.Url -> Party
parseParty url =
    case parse matchers url of
        Just party ->
            party

        Nothing ->
            Prosecution



matchers : Parser (Party -> a) a
matchers =
    oneOf
        [ map Prosecution (s "Prosecution")
        , map Prosecution top
        , map Defense (s "Defense")
        , map Prosecution (s "Clear")
        ]


fromValue : Nav.Key -> Value -> Model
fromValue key value =
    let
        result =
            Decode.decodeValue (modelDecoder key) value
    in
    case result of
        Ok model ->
            model

        Err _ ->
            initModel Prosecution key


fromJson : Url.Url -> Nav.Key -> String -> Model
fromJson url key json =
    let
        result =
            Decode.decodeString (modelDecoder key) json
    in
    case result of
        Ok model ->
            model

        Err _ ->
            initModel Prosecution key


modelDecoder : Nav.Key -> Decoder Model
modelDecoder key =
    Decode.succeed Model
        |> hardcoded key
        |> required "prosecution" CaseInChief.caseInChiefDecoder
        |> required "defense" CaseInChief.caseInChiefDecoder
        |> required "clearDialogIsActive" Decode.bool
        |> required "currentTab" partyDecoder


partyDecoder : Decoder Party
partyDecoder =
    Decode.string
        |> Decode.andThen stringToPartyDecoder


stringToPartyDecoder : String -> Decoder Party
stringToPartyDecoder string =
    case string of
        "Defense" ->
            Decode.succeed Defense

        _ ->
            Decode.succeed Prosecution



---- UPDATE ----


type Msg
    = NoOp
    | ClearModel
    | LinkClicked Browser.UrlRequest
    | ReceiveCache Value
    | ToggleClearDialog
    | UpdateProsecutionDirect WitnessExam String
    | UpdateProsecutionCross WitnessExam String
    | UpdateProsecutionRedirect WitnessExam String
    | UpdateDefenseDirect WitnessExam String
    | UpdateDefenseCross WitnessExam String
    | UpdateDefenseRedirect WitnessExam String
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | party = parseParty url }, Cmd.none )

        ClearModel ->
            let
                model_ =
                    initModel Prosecution model.key
            in
            ( model_, saveData model_ )

        ReceiveCache value ->
            ( fromValue model.key value, Cmd.none )

        ToggleClearDialog ->
            let
                model_ =
                    toggleClearDialog model
            in
            ( model_, Cmd.none )

        UpdateProsecutionDirect witnessExam string ->
            let
                model_ =
                    updateExam string Prosecution Direct witnessExam model
            in
            ( model_, saveData model_ )

        UpdateProsecutionCross witnessExam string ->
            let
                model_ =
                    updateExam string Prosecution Cross witnessExam model
            in
            ( model_, saveData model_ )

        UpdateProsecutionRedirect witnessExam string ->
            let
                model_ =
                    updateExam string Prosecution Redirect witnessExam model
            in
            ( model_, saveData model_ )

        UpdateDefenseDirect witnessExam string ->
            let
                model_ =
                    updateExam string Defense Direct witnessExam model
            in
            ( model_, saveData model_ )

        UpdateDefenseCross witnessExam string ->
            let
                model_ =
                    updateExam string Defense Cross witnessExam model
            in
            ( model_, saveData model_ )

        UpdateDefenseRedirect witnessExam string ->
            let
                model_ =
                    updateExam string Defense Redirect witnessExam model
            in
            ( model_, saveData model_ )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "RMT Timer | " ++ partyString model.party
    , body =
        [ div [ class "section" ]
            [ viewTabs model
            , viewPartyCaseInChief model
            , viewClearDialog model.clearDialogIsActive
            ]
        ]
    }


viewTabs : Model -> Html Msg
viewTabs model =
    div [ class "tabs is-boxed" ]
        [ ul []
            [ partyTab Prosecution model.party
            , partyTab Defense model.party
            , clearDialogTab model
            ]
        ]


partyTab : Party -> Party -> Html Msg
partyTab party activeParty =
    let
        isActive =
            party == activeParty
    in
    li [ classList [ ( "is-active", isActive ) ] ]
        [ a [ href (partyString party) ]
            [ partyHtml party ]
        ]


clearDialogTab : Model -> Html Msg
clearDialogTab model =
    if
        model.prosecution
            == CaseInChief.new [ "P1", "P2", "P3", "P4" ]
            && model.defense
            == CaseInChief.new [ "D1", "D2", "D3", "D4" ]
    then
        text ""

    else
        li []
            [ a [ class "has-text-danger", onClick ToggleClearDialog ]
                [ span [ class "icon is-small" ] [ i [ class "fas fa-trash-alt" ] [] ]
                , span [] [ text "Clear" ]
                ]
            ]


viewClearDialog : Bool -> Html Msg
viewClearDialog clearDialogIsActive =
    div [ classList [ ( "modal", True ), ( "is-active", clearDialogIsActive ) ] ]
        [ div [ class "modal-background", onClick ToggleClearDialog ] []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ]
                    [ text "Are you sure?" ]
                , button [ attribute "aria-label" "close", class "delete", onClick ToggleClearDialog ] []
                ]
            , section [ class "modal-card-body" ]
                [ text "This will clear the worksheet." ]
            , footer [ class "modal-card-foot", style "justify-content" "flex-end" ]
                [ button [ class "button is-danger", onClick ClearModel ]
                    [ text "Clear" ]
                , button [ class "button", onClick ToggleClearDialog ]
                    [ text "Cancel" ]
                ]
            ]
        ]


viewPartyCaseInChief : Model -> Html Msg
viewPartyCaseInChief model =
    div [ class "container" ]
        [ viewCaseInChiefTitle model.party
        , viewExaminations model
        , viewRemainingDirect model
        , viewRemainingCross model
        ]


viewCaseInChiefTitle : Party -> Html msg
viewCaseInChiefTitle party =
    div [ class "title is-5" ] [ partyHtml party, text " witnesses" ]


viewExaminations : Model -> Html Msg
viewExaminations model =
    table [ class "table" ]
        [ viewExaminationsHead
        , viewExaminationsBody model
        ]


viewExaminationsHead : Html Msg
viewExaminationsHead =
    thead []
        [ th [ class "has-text-centered" ] [ text "Direct" ]
        , th [ class "has-text-centered" ] [ text "Cross" ]
        , th [ class "has-text-centered" ] [ text "Redirect" ]
        ]


viewExaminationsBody : Model -> Html Msg
viewExaminationsBody model =
    case model.party of
        Prosecution ->
            tbody [] (viewProsecutionRows model.prosecution)

        Defense ->
            tbody [] (viewDefenseRows model.defense)


viewProsecutionRows : CaseInChief -> List (Html Msg)
viewProsecutionRows caseInChief_ =
    caseInChief_
        |> CaseInChief.toList
        |> List.map viewProsecutionRow


viewProsecutionRow : WitnessExam -> Html Msg
viewProsecutionRow exam =
    viewExamination exam
        ( UpdateProsecutionDirect exam
        , UpdateProsecutionCross exam
        , UpdateProsecutionRedirect exam
        )


viewDefenseRows : CaseInChief -> List (Html Msg)
viewDefenseRows caseInChief_ =
    caseInChief_
        |> CaseInChief.toList
        |> List.map viewDefenseRow


viewDefenseRow : WitnessExam -> Html Msg
viewDefenseRow exam =
    viewExamination exam
        ( UpdateDefenseDirect exam
        , UpdateDefenseCross exam
        , UpdateDefenseRedirect exam
        )


viewExamination : WitnessExam -> ( String -> Msg, String -> Msg, String -> Msg ) -> Html Msg
viewExamination ( _, exam ) ( directMsg, crossMsg, redirectMsg ) =
    tr []
        [ td [] [ inputDuration (Exam.directString exam) directMsg ]
        , td [] [ inputDuration (Exam.crossString exam) crossMsg ]
        , td [] [ inputDuration (Exam.redirectString exam) redirectMsg ]
        ]


inputDuration : String -> (String -> Msg) -> Html Msg
inputDuration string msg =
    let
        value_ =
            if string == "0:00" then
                ""

            else
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


viewRemainingDirect : Model -> Html Msg
viewRemainingDirect model =
    model
        |> caseInChief model.party
        |> CaseInChief.totalCombinedDirect
        |> viewRemaining model.party Direct 14


viewRemainingCross : Model -> Html Msg
viewRemainingCross model =
    model
        |> caseInChief model.party
        |> CaseInChief.totalCross
        |> viewRemaining (opponent model.party) Cross 10


viewRemaining : Party -> ExamType -> Int -> Duration -> Html Msg
viewRemaining party examType minutes totalDuration =
    let
        wrap string =
            text ("\u{00A0}" ++ string ++ "\u{00A0}")

        used =
            totalDuration
                |> Duration.toString

        remaining =
            totalDuration
                |> Duration.subtract (minutes |> Duration.fromMinutes)
                |> Duration.toString

        twoMinuteWarning =
            totalDuration
                |> Duration.subtract (minutes - 2 |> Duration.fromMinutes)
                |> Duration.toString

        oneMinuteWarning =
            totalDuration
                |> Duration.subtract (minutes - 1 |> Duration.fromMinutes)
                |> Duration.toString

        thirtySecondWarning =
            totalDuration
                |> Duration.subtract (minutes * 60 - 30 |> Duration.fromSeconds)
                |> Duration.toString

        spanBold html =
            span [ class "has-text-weight-bold" ] html
    in
    p []
        [ spanBold [ partyHtml party ]
        , span [] [ text " has used " ]
        , spanBold [ text used ]
        , span [] [ text " on " ]
        , spanBold [ text (examTypeToString examType) ]
        , span [] [ text " and has " ]
        , spanBold [ text remaining ]
        , span [] [ text " remaining. " ]
        , span [ class "has-text-black has-text-weight-bold has-background-info" ]
            [ wrap twoMinuteWarning ]
        , span [ class "has-text-black has-text-weight-bold has-background-primary" ]
            [ wrap oneMinuteWarning ]
        , span [ class "has-text-black has-text-weight-bold has-background-warning" ]
            [ wrap thirtySecondWarning ]
        , span [ class "has-text-white has-text-weight-bold has-background-danger" ]
            [ wrap remaining ]
        ]



---- INTEROP ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveData ReceiveCache


port receiveData : (Value -> msg) -> Sub msg


port requestData : Maybe String -> Cmd msg


port sendData : Value -> Cmd msg


saveData : Model -> Cmd Msg
saveData model =
    model
        |> encodeModel
        |> sendData


encodeModel : Model -> Value
encodeModel model =
    object
        [ ( "prosecution", CaseInChief.encodeCaseInChief model.prosecution )
        , ( "defense", CaseInChief.encodeCaseInChief model.defense )
        , ( "clearDialogIsActive", Encode.bool model.clearDialogIsActive )
        , ( "currentTab", encodeParty model.party )
        ]


encodeParty : Party -> Value
encodeParty party =
    case party of
        Defense ->
            Encode.string "Defense"

        _ ->
            Encode.string "Prosecution"



---- HELPERS ----


examTypeToString : ExamType -> String
examTypeToString examType =
    case examType of
        Direct ->
            "Direct"

        Cross ->
            "Cross"

        Redirect ->
            "Redirect"


partyHtml : Party -> Html msg
partyHtml party =
    party
        |> partyString
        |> text


partyString : Party -> String
partyString party =
    case party of
        Prosecution ->
            "Prosecution"

        Defense ->
            "Defense"


opponent : Party -> Party
opponent party =
    case party of
        Prosecution ->
            Defense

        Defense ->
            Prosecution


caseInChief : Party -> Model -> CaseInChief
caseInChief party model =
    case party of
        Prosecution ->
            model.prosecution

        Defense ->
            model.defense


toggleClearDialog : Model -> Model
toggleClearDialog model =
    { model | clearDialogIsActive = not model.clearDialogIsActive }


toggleParty : Model -> Model
toggleParty model =
    { model | party = opponent model.party }


updateDirect : String -> WitnessExam -> CaseInChief -> CaseInChief
updateDirect string witnessExam caseInChief_ =
    CaseInChief.updateDirectWithString string witnessExam caseInChief_


updateCross : String -> WitnessExam -> CaseInChief -> CaseInChief
updateCross string witnessExam caseInChief_ =
    CaseInChief.updateCrossWithString string witnessExam caseInChief_


updateRedirect : String -> WitnessExam -> CaseInChief -> CaseInChief
updateRedirect string witnessExam caseInChief_ =
    CaseInChief.updateRedirectWithString string witnessExam caseInChief_


updateExam : String -> Party -> ExamType -> WitnessExam -> Model -> Model
updateExam string party examType witnessExam model =
    case ( party, examType ) of
        ( Prosecution, Direct ) ->
            { model | prosecution = updateDirect string witnessExam model.prosecution }

        ( Prosecution, Cross ) ->
            { model | prosecution = updateCross string witnessExam model.prosecution }

        ( Prosecution, Redirect ) ->
            { model | prosecution = updateRedirect string witnessExam model.prosecution }

        ( Defense, Direct ) ->
            { model | defense = updateDirect string witnessExam model.defense }

        ( Defense, Cross ) ->
            { model | defense = updateCross string witnessExam model.defense }

        ( Defense, Redirect ) ->
            { model | defense = updateRedirect string witnessExam model.defense }
