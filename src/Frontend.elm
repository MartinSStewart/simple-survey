module Frontend exposing (..)

import AssocList as Dict
import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Effect.Browser.Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import Element exposing (Element)
import Element.Background
import Element.Font
import Element.Input
import EmailAddress exposing (EmailAddress)
import Html
import Html.Attributes as Attr
import IdDict
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
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
                        { questions = Nonempty "" []
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
                            ( { model | state = AnsweringSurvey { answeringSurvey | submitState = Submitting () } }
                            , List.Nonempty.map (\{ answer } -> { answer = answer }) answeringSurvey.answers
                                |> SubmitSurveyRequest answeringSurvey.surveyId answeringSurvey.userToken
                                |> Effect.Lamdera.sendToBackend
                            )

                        Submitting () ->
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
                                                List.Nonempty.toList answeringSurvey.answers
                                                    |> List.updateAt index (\a -> { a | answer = text })
                                                    |> List.Nonempty.fromList
                                                    |> Maybe.withDefault answeringSurvey.answers
                                        }
                              }
                            , Command.none
                            )

                        Submitting () ->
                            ( model, Command.none )

                _ ->
                    ( model, Command.none )


validateEmails : String -> Result String (Nonempty EmailAddress)
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
            case List.Nonempty.fromList validEmails of
                Just nonempty ->
                    Ok nonempty

                Nothing ->
                    Err "Include at least one email address. Otherwise the survey won't be shown to anyone!"

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
                            ( { creatingSurvey
                                | submitState =
                                    Submitting { questions = creatingSurvey.questions, emailTo = emails }
                              }
                            , CreateSurveyRequest
                                (List.Nonempty.map (\question -> { question = question }) creatingSurvey.questions)
                                emails
                                |> Effect.Lamdera.sendToBackend
                            )

                        Err _ ->
                            ( { creatingSurvey | submitState = NotSubmitted HasPressedSubmitted }
                            , Command.none
                            )

                Submitting _ ->
                    ( creatingSurvey, Command.none )

        PressedAddQuestion ->
            ( { creatingSurvey | questions = List.Nonempty.append creatingSurvey.questions (Nonempty "" []) }
            , Command.none
            )

        PressedRemoveQuestion index ->
            ( { creatingSurvey
                | questions =
                    List.Nonempty.toList creatingSurvey.questions
                        |> List.removeAt index
                        |> List.Nonempty.fromList
                        |> Maybe.withDefault creatingSurvey.questions
              }
            , Command.none
            )

        PressedMoveUpQuestion index ->
            ( { creatingSurvey
                | questions =
                    List.Nonempty.toList creatingSurvey.questions
                        |> List.swapAt index (index - 1)
                        |> List.Nonempty.fromList
                        |> Maybe.withDefault creatingSurvey.questions
              }
            , Command.none
            )

        PressedMoveDownQuestion index ->
            ( { creatingSurvey
                | questions =
                    List.Nonempty.toList creatingSurvey.questions
                        |> List.swapAt index (index + 1)
                        |> List.Nonempty.fromList
                        |> Maybe.withDefault creatingSurvey.questions
              }
            , Command.none
            )

        TypedQuestion index text ->
            ( { creatingSurvey
                | questions =
                    List.Nonempty.toList creatingSurvey.questions
                        |> List.setAt index text
                        |> List.Nonempty.fromList
                        |> Maybe.withDefault creatingSurvey.questions
              }
            , Command.none
            )

        TypedEmailTo text ->
            ( { creatingSurvey | emailTo = text }, Command.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Command FrontendOnly toMsg FrontendMsg )
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
                CreatingSurvey { submitState } ->
                    case submitState of
                        Submitting { questions, emailTo } ->
                            ( { model
                                | state =
                                    SurveyOverviewAdmin
                                        surveyId
                                        { questions =
                                            List.Nonempty.map
                                                (\question -> { question = question, answers = Dict.empty })
                                                questions
                                        , emailedTo = emailTo
                                        }
                              }
                            , Effect.Browser.Navigation.pushUrl
                                model.key
                                (Route.encode (Route.ViewSurvey surveyId userToken))
                            )

                        NotSubmitted _ ->
                            ( model, Command.none )

                _ ->
                    ( model, Command.none )

        LoadSurveyResponse surveyId userToken result ->
            ( case result of
                Ok ok ->
                    { model
                        | state =
                            AnsweringSurvey
                                { surveyId = surveyId
                                , userToken = userToken
                                , answers =
                                    List.Nonempty.map
                                        (\{ question } -> { question = question, answer = "" })
                                        ok
                                , submitState = NotSubmitted HasNotPressedSubmitted
                                }
                    }

                Err error ->
                    { model | state = LoadingSurveyFailed error }
            , Command.none
            )

        LoadSurveyAdminResponse surveyId result ->
            ( case result of
                Ok ok ->
                    { model | state = SurveyOverviewAdmin surveyId ok }

                Err () ->
                    { model | state = LoadingSurveyFailed InvalidSurveyLink }
            , Command.none
            )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout
            []
            (case model.state of
                LoadingSurvey _ _ ->
                    Element.text "Loading..."

                AnsweringSurvey answeringSurvey2 ->
                    Element.text ""

                SubmittedSurvey ->
                    Element.paragraph
                        [ Element.centerX, Element.centerY ]
                        [ Element.text "Survey successfully submitted. Thanks!" ]

                CreatingSurvey creatingSurvey2 ->
                    Element.column
                        []
                        [ Element.column
                            [ Element.spacing 32 ]
                            (List.indexedMap editQuestionView (List.Nonempty.toList creatingSurvey2.questions))
                        , Element.Input.text
                            []
                            { onChange = TypedEmailTo
                            , text = creatingSurvey2.emailTo
                            , placeholder = Nothing
                            , label =
                                Element.Input.labelAbove
                                    []
                                    (Element.paragraph
                                        []
                                        [ Element.text "List of people you want this survey emailed to. Separate each email with a comma (i.e. john.doe@example.com, bob-smith@bob.com, jane123@mail.com)"
                                        ]
                                    )
                            }
                        ]
                        |> Element.map CreateSurveyMsg

                SurveyOverviewAdmin id backendSurvey ->
                    Element.text ""

                LoadingSurveyFailed loadSurveyError ->
                    Element.text ""
            )
        ]
    }


editQuestionView : Int -> String -> Element CreateSurveyMsg
editQuestionView index text =
    Element.row
        [ Element.spacing 8 ]
        [ Element.Input.multiline
            []
            { onChange = TypedQuestion index
            , text = text
            , placeholder = Nothing
            , label =
                Element.Input.labelAbove
                    []
                    (Element.text ("Question " ++ String.fromInt (index + 1) ++ "."))
            , spellcheck = True
            }
        , Element.row
            []
            [ button [] (PressedMoveDownQuestion index) (Element.text "▼")
            , button [] (PressedMoveUpQuestion index) (Element.text "▲")
            ]
        , button
            [ Element.Background.color (Element.rgb 1 0 0)
            , Element.Font.color (Element.rgb 1 1 1)
            ]
            (PressedRemoveQuestion index)
            (Element.text "×")
        ]


button attributes msg label =
    Element.Input.button attributes { onPress = Just msg, label = label }
