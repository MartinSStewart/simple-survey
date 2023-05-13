module Backend exposing (..)

import AssocList as Dict
import Duration
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task as Task
import Effect.Time as Time
import Email.Html
import Email.Html.Attributes
import Env
import Hex
import Id exposing (Id, SurveyId, UserToken)
import IdDict
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Postmark
import Quantity
import Route
import String.Nonempty exposing (NonemptyString(..))
import Survey exposing (EmailStatus(..))
import SurveyName
import Types exposing (..)
import Unsafe


app =
    Effect.Lamdera.backend
        Lamdera.broadcast
        Lamdera.sendToFrontend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Time.every Duration.hour HourElapsed
        }


init : ( BackendModel, Command restriction toMsg BackendMsg )
init =
    ( { surveys = IdDict.empty, secretCounter = 0, surveyIdCounter = 0 }
    , Command.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        SurveyEmailSent surveyId emailAddress result ->
            ( { model
                | surveys =
                    IdDict.update
                        surveyId
                        (Maybe.map
                            (\survey ->
                                { survey
                                    | emailedTo =
                                        List.Nonempty.map
                                            (\(( userToken, { email, emailStatus } ) as original) ->
                                                if emailAddress == email then
                                                    ( userToken
                                                    , { email = email
                                                      , emailStatus =
                                                            case result of
                                                                Ok () ->
                                                                    EmailSuccess

                                                                Err error ->
                                                                    EmailError error
                                                      }
                                                    )

                                                else
                                                    original
                                            )
                                            survey.emailedTo
                                }
                            )
                        )
                        model.surveys
              }
            , Command.none
            )

        GotTime time sessionId clientId toBackend ->
            updateFromFrontendWithTime time sessionId clientId toBackend model

        HourElapsed time ->
            ( { model
                | surveys =
                    IdDict.filter
                        (\_ survey -> Duration.from survey.creationTime time |> Quantity.lessThan (Duration.days 90))
                        model.surveys
              }
            , Command.none
            )


surveyEmail :
    SurveyName.SurveyName
    -> Id SurveyId
    -> Id UserToken
    -> { subject : NonemptyString, htmlBody : Email.Html.Html, textBody : String }
surveyEmail surveyName surveyId userToken =
    let
        link =
            Env.domain ++ Route.encode (Route.ViewSurvey surveyId userToken)
    in
    { subject = SurveyName.toNonemptyString surveyName
    , htmlBody =
        Email.Html.div
            []
            [ Email.Html.text "A survey titled "
            , Email.Html.b
                []
                [ Email.Html.text (SurveyName.toString surveyName) ]
            , Email.Html.text " is ready for you to fill out. "
            , Email.Html.a
                [ Email.Html.Attributes.href link ]
                [ Email.Html.text "Click here to view it." ]
            , Email.Html.br [] []
            , Email.Html.br [] []
            , Email.Html.text "If you don't recognize this survey then we recommend not filling it out."
            ]
    , textBody = "A survey titled \"" ++ SurveyName.toString surveyName ++ "\" is ready for you to fill out. Use this link to view it: " ++ link ++ ". If you don't recognize this survey then we recommend not filling it out."
    }


updateFromFrontend :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    ( model, Time.now |> Task.perform (\time -> GotTime time sessionId clientId msg) )


updateFromFrontendWithTime :
    Time.Posix
    -> SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontendWithTime time sessionId clientId msg model =
    case msg of
        SubmitSurveyRequest surveyId userToken answers ->
            case IdDict.get surveyId model.surveys of
                Just survey ->
                    case nonemptyGet userToken survey.emailedTo of
                        Just { email } ->
                            let
                                answers2 : Nonempty { answer : String }
                                answers2 =
                                    List.Nonempty.toList answers
                                        ++ List.repeat (List.Nonempty.length survey.questions) { answer = "" }
                                        |> List.Nonempty.fromList
                                        |> Maybe.withDefault (Nonempty { answer = "" } [])
                            in
                            ( { model
                                | surveys =
                                    IdDict.insert
                                        surveyId
                                        { survey
                                            | questions =
                                                List.Nonempty.map2
                                                    (\question newAnswer ->
                                                        case String.Nonempty.fromString newAnswer.answer of
                                                            Just answer ->
                                                                { question
                                                                    | answers =
                                                                        Dict.insert email answer question.answers
                                                                }

                                                            Nothing ->
                                                                question
                                                    )
                                                    survey.questions
                                                    answers2
                                        }
                                        model.surveys
                              }
                            , Effect.Lamdera.sendToFrontend clientId SubmitSurveyResponse
                            )

                        Nothing ->
                            ( model, Command.none )

                Nothing ->
                    ( model, Command.none )

        CreateSurveyRequest surveyName questions emailTo ->
            let
                ( model2, surveyId ) =
                    ( { model | surveyIdCounter = model.surveyIdCounter + 1 }
                    , Hex.toString model.surveyIdCounter |> Id.fromString
                    )

                ( model3, userToken ) =
                    Id.getUniqueId model2

                ( model4, userToken2 ) =
                    Id.getUniqueId model3

                ( model7, emailTo2 ) =
                    List.foldl
                        (\email ( model5, list ) ->
                            let
                                ( model6, userToken3 ) =
                                    Id.getUniqueId model5
                            in
                            ( model6
                            , List.Nonempty.cons ( userToken3, { email = email, emailStatus = SendingEmail } ) list
                            )
                        )
                        ( model4
                        , Nonempty ( userToken2, { email = List.Nonempty.head emailTo, emailStatus = SendingEmail } ) []
                        )
                        (List.Nonempty.tail emailTo)

                emails : Command restriction toMsg BackendMsg
                emails =
                    List.Nonempty.toList emailTo2
                        |> List.map
                            (\( userToken3, { email } ) ->
                                let
                                    { subject, htmlBody, textBody } =
                                        surveyEmail surveyName surveyId userToken3

                                    _ =
                                        Debug.log "login" (Env.domain ++ Route.encode (Route.ViewSurvey surveyId userToken3))
                                in
                                Postmark.sendEmail
                                    (SurveyEmailSent surveyId email)
                                    Env.postmarkApiKey
                                    { from = { name = "Simple Survey", email = replyEmail }
                                    , to = Nonempty { name = "", email = email } []
                                    , subject = subject
                                    , body = Postmark.BodyBoth htmlBody textBody
                                    , messageStream = "outbound"
                                    }
                            )
                        |> Command.batch
            in
            ( { model7
                | surveys =
                    IdDict.insert
                        surveyId
                        { title = surveyName
                        , questions =
                            List.Nonempty.map
                                (\{ question } -> { question = question, answers = Dict.empty })
                                questions
                        , emailedTo = emailTo2
                        , owner = userToken
                        , creationTime = time
                        }
                        model7.surveys
              }
            , Command.batch
                [ Effect.Lamdera.sendToFrontend clientId (CreateSurveyResponse surveyId userToken emailTo2 time)
                , emails
                ]
            )

        LoadSurveyRequest surveyId userToken ->
            ( model
            , (case IdDict.get surveyId model.surveys of
                Just survey ->
                    if survey.owner == userToken then
                        { title = survey.title
                        , questions = survey.questions
                        , emailedTo = survey.emailedTo
                        , owner = survey.owner
                        , creationTime = survey.creationTime
                        }
                            |> Ok
                            |> LoadSurveyAdminResponse surveyId

                    else
                        case nonemptyGet userToken survey.emailedTo of
                            Just { email } ->
                                if Survey.hasSubmitted email survey then
                                    LoadSurveyResponse (Err SurveyAlreadySubmitted)

                                else
                                    { surveyId = surveyId
                                    , userToken = userToken
                                    , emailAddress = email
                                    , surveyName = survey.title
                                    , questions = List.Nonempty.map (\{ question } -> { question = question }) survey.questions
                                    , creationTime = survey.creationTime
                                    }
                                        |> Ok
                                        |> LoadSurveyResponse

                            Nothing ->
                                LoadSurveyResponse (Err InvalidSurveyLink)

                Nothing ->
                    LoadSurveyResponse (Err InvalidSurveyLink)
              )
                |> Effect.Lamdera.sendToFrontend clientId
            )


replyEmail =
    Unsafe.emailAddress "no-reply@simple-survey.lamdera.app"


nonemptyGet : a -> Nonempty ( a, b ) -> Maybe b
nonemptyGet a nonempty =
    List.Nonempty.toList nonempty
        |> List.findMap
            (\( a2, b ) ->
                if a2 == a then
                    Just b

                else
                    Nothing
            )
