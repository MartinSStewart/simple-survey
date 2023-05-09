module Backend exposing (..)

import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import Html
import IdDict
import Lamdera
import Types exposing (..)


app =
    Effect.Lamdera.backend
        Lamdera.broadcast
        Lamdera.sendToFrontend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Subscription.none
        }


init : ( BackendModel, Command restriction toMsg BackendMsg )
init =
    ( { surveys = IdDict.empty }
    , Command.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Command restriction toMsg BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Command.none )


updateFromFrontend :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        SubmitSurveyRequest answers ->
            ( model, Effect.Lamdera.sendToFrontend clientId SubmitSurveyResponse )

        CreateSurveyRequest questions ->
            ()

        LoadSurveyRequest surveyId userToken ->
            ()

        LoadSurveyAdminRequest surveyId adminToken ->
            ()
