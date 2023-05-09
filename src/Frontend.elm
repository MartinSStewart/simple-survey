module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Effect.Browser.Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import EmailAddress exposing (EmailAddress)
import Html
import Html.Attributes as Attr
import IdDict
import Lamdera
import List.Extra as List
import Route
import Types exposing (..)
import Url
import Url.Parser


app =
    Effect.Lamdera.frontend
        Lamdera.sendToBackend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Subscription.none
        , view = view
        }


init : Url.Url -> Effect.Browser.Navigation.Key -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
init url key =
    let
        route =
            Url.Parser.parse Route.decode url |> Maybe.withDefault Route.CreateSurvey
    in
    case route of
        Route.CreateSurvey ->
            ( { key = key
              , state =
                    CreatingSurvey
                        { questions = []
                        , submitState = NotSubmitted HasNotPressedSubmitted
                        , emailTo = ""
                        }
              }
            , Command.none
            )

        Route.ViewSurvey surveyId userToken ->
            ( { key = key, state = LoadingSurvey surveyId userToken }
            , LoadSurveyRequest surveyId userToken |> Effect.Lamdera.sendToBackend
            )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Effect.Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Effect.Browser.Navigation.load url
                    )

        UrlChanged url ->
            ( model, Command.none )

        PressedSubmitSurvey ->
            case model.state of
                AnsweringSurvey answeringSurvey ->
                    case answeringSurvey.submitState of
                        NotSubmitted _ ->
                            ( { model | state = AnsweringSurvey { answeringSurvey | submitState = Submitting } }
                            , List.map (\{ answer } -> { answer = answer }) answeringSurvey.answers
                                |> SubmitSurveyRequest
                                |> Effect.Lamdera.sendToBackend
                            )

                        Submitting ->
                            ( model, Command.none )

                _ ->
                    ( model, Command.none )

        CreateSurveyMsg createSurveyMsg ->
            case model.state of
                CreatingSurvey creatingSurvey ->
                    updateCreateSurvey createSurveyMsg creatingSurvey
                        |> Tuple.mapFirst (\a -> { model | state = CreatingSurvey a })

                _ ->
                    ( model, Command.none )

        TypedAnswer index text ->
            case model.state of
                AnsweringSurvey answeringSurvey ->
                    case answeringSurvey.submitState of
                        NotSubmitted _ ->
                            ( { model
                                | state =
                                    AnsweringSurvey
                                        { answeringSurvey
                                            | answers =
                                                List.updateAt index (\a -> { a | answer = text }) answeringSurvey.answers
                                        }
                              }
                            , Command.none
                            )

                        Submitting ->
                            ( model, Command.none )

                _ ->
                    ( model, Command.none )


validateEmails : String -> Result String (List EmailAddress)
validateEmails text =
    let
        emails : List ( String, Maybe EmailAddress )
        emails =
            String.split "," text |> List.map (\subtext -> ( subtext, EmailAddress.fromString subtext ))

        validEmails : List EmailAddress
        validEmails =
            List.filterMap Tuple.second emails

        invalidEmails : List String
        invalidEmails =
            List.filterMap
                (\( subtext, maybeValid ) ->
                    if maybeValid == Nothing then
                        Just subtext

                    else
                        Nothing
                )
                emails
    in
    case invalidEmails of
        [] ->
            Ok validEmails

        [ invalidEmail ] ->
            invalidEmail ++ " is not a valid email" |> Err

        _ ->
            invalidEmails
                |> String.join ", "
                |> (\a -> a ++ " are not valid emails")
                |> Err


updateCreateSurvey : CreateSurveyMsg -> CreatingSurvey2 -> ( CreatingSurvey2, Command FrontendOnly ToBackend FrontendMsg )
updateCreateSurvey msg creatingSurvey =
    case msg of
        PressedCreateSurvey ->
            case creatingSurvey.submitState of
                NotSubmitted _ ->
                    case validateEmails creatingSurvey.emailTo of
                        Ok emails ->
                            ( { creatingSurvey | submitState = Submitting }
                            , CreateSurveyRequest
                                (List.map (\question -> { question = question }) creatingSurvey.questions)
                                emails
                                |> Effect.Lamdera.sendToBackend
                            )

                        Err _ ->
                            ( { creatingSurvey | submitState = NotSubmitted HasPressedSubmitted }
                            , Command.none
                            )

                Submitting ->
                    ( creatingSurvey, Command.none )

        PressedAddQuestion ->
            ( { creatingSurvey | questions = creatingSurvey.questions ++ [ "" ] }
            , Command.none
            )

        PressedRemoveQuestion index ->
            ( { creatingSurvey | questions = List.removeAt index creatingSurvey.questions }
            , Command.none
            )

        PressedMoveUpQuestion index ->
            ( { creatingSurvey | questions = List.swapAt index (index - 1) creatingSurvey.questions }
            , Command.none
            )

        PressedMoveDownQuestion index ->
            ( { creatingSurvey | questions = List.swapAt index (index + 1) creatingSurvey.questions }
            , Command.none
            )

        TypedQuestion index text ->
            ( { creatingSurvey | questions = List.setAt index text creatingSurvey.questions }
            , Command.none
            )

        TypedEmailRecipients text ->
            ( { creatingSurvey | emailTo = text }, Command.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Command restriction toMsg FrontendMsg )
updateFromBackend msg model =
    case msg of
        SubmitSurveyResponse ->
            case model.state of
                AnsweringSurvey _ ->
                    ( { model | state = SubmittedSurvey }, Command.none )

                _ ->
                    ( model, Command.none )

        CreateSurveyResponse surveyId userToken ->
            case model.state of
                CreatingSurvey { questions } ->
                    ( { model
                        | state =
                            SurveyOverviewAdmin
                                surveyId
                                { questions =
                                    List.map
                                        (\question -> { question = question, answers = IdDict.empty })
                                        questions
                                , emailedTo = IdDict.empty
                                , owner = userToken
                                }
                      }
                    , Command.none
                    )

                _ ->
                    ( model, Command.none )

        LoadSurveyResponse surveyId result ->
            ( case result of
                Ok ok ->
                    { model
                        | state =
                            AnsweringSurvey
                                { surveyId = surveyId
                                , questions =
                                    List.map
                                        (\{ question } -> { question = question, answer = "" })
                                        ok
                                , submitState = NotSubmitted HasNotPressedSubmitted
                                }
                    }

                Err error ->
                    { model | state = LoadingSurveyFailed error }
            , Command.none
            )

        LoadSurveyAdminResponse result ->
            0


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        []
    }
