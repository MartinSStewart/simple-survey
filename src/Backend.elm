module Backend exposing (..)

import AssocList as Dict
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import EmailAddress exposing (EmailAddress)
import Html
import Id
import IdDict
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import String.Nonempty
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
    ( { surveys = IdDict.empty, secretCounter = 0 }
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

        CreateSurveyRequest questions emailTo ->
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
                            , List.Nonempty.cons ( userToken3, { email = email, result = SendingEmail } ) list
                            )
                        )
                        ( model4, Nonempty ( userToken2, { email = List.Nonempty.head emailTo, result = SendingEmail } ) [] )
                        (List.Nonempty.tail emailTo)
            in
            ( { model7
                | surveys =
                    IdDict.insert
                        surveyId
                        { questions =
                            List.Nonempty.map
                                (\{ question } -> { question = question, answers = Dict.empty })
                                questions
                        , emailedTo = emailTo2
                        , owner = userToken
                        }
                        model7.surveys
              }
            , Effect.Lamdera.sendToFrontend clientId (CreateSurveyResponse surveyId userToken)
            )

        LoadSurveyRequest surveyId userToken ->
            ( model
            , (case IdDict.get surveyId model.surveys of
                Just survey ->
                    if survey.owner == userToken then
                        { questions = survey.questions
                        , emailedTo = List.Nonempty.map Tuple.second survey.emailedTo
                        }
                            |> Ok
                            |> LoadSurveyAdminResponse surveyId

                    else
                        case nonemptyGet userToken survey.emailedTo of
                            Just { email } ->
                                if hasSubmitted email survey then
                                    List.Nonempty.map (\{ question } -> { question = question }) survey.questions
                                        |> Ok
                                        |> LoadSurveyResponse surveyId userToken

                                else
                                    LoadSurveyResponse surveyId userToken (Err InvalidSurveyLink)

                            Nothing ->
                                LoadSurveyResponse surveyId userToken (Err InvalidSurveyLink)

                Nothing ->
                    LoadSurveyResponse surveyId userToken (Err InvalidSurveyLink)
              )
                |> Effect.Lamdera.sendToFrontend clientId
            )


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
