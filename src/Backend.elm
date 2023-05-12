module Backend exposing (..)

import AssocList as Dict
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Http as Http
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task as Task exposing (Task)
import Email.Html
import EmailAddress exposing (EmailAddress)
import Env
import Html
import Id
import IdDict
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Postmark
import String.Nonempty exposing (NonemptyString(..))
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
        , subscriptions = \m -> Subscription.none
        }


init : ( BackendModel, Command restriction toMsg BackendMsg )
init =
    ( { surveys = IdDict.empty, secretCounter = 0 }
    , Command.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Command restriction toMsg BackendMsg )
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
                                                                Ok ok ->
                                                                    EmailSuccess ok

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


updateFromFrontend :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
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
                    Id.getUniqueId model

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
                    List.Nonempty.toList emailTo
                        |> List.map
                            (\email ->
                                Postmark.sendEmail
                                    (SurveyEmailSent surveyId email)
                                    Env.postmarkApiKey
                                    { from = { name = "Simple Survey", email = replyEmail }
                                    , to = Nonempty { name = "", email = email } []
                                    , subject = SurveyName.toNonemptyString surveyName
                                    , body =
                                        Postmark.BodyBoth
                                            (Email.Html.text "")
                                            ""
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
                        }
                        model7.surveys
              }
            , Command.batch
                [ Effect.Lamdera.sendToFrontend clientId (CreateSurveyResponse surveyId userToken emailTo2)
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
                        }
                            |> Ok
                            |> LoadSurveyAdminResponse surveyId

                    else
                        case nonemptyGet userToken survey.emailedTo of
                            Just { email } ->
                                if hasSubmitted email survey then
                                    { surveyId = surveyId
                                    , userToken = userToken
                                    , emailAddress = email
                                    , surveyName = survey.title
                                    , questions = List.Nonempty.map (\{ question } -> { question = question }) survey.questions
                                    }
                                        |> Ok
                                        |> LoadSurveyResponse

                                else
                                    LoadSurveyResponse (Err InvalidSurveyLink)

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


hasSubmitted : EmailAddress -> BackendSurvey -> Bool
hasSubmitted emailAddress survey =
    List.Nonempty.any (\{ answers } -> Dict.keys answers |> List.any ((==) emailAddress)) survey.questions
