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
import Element.Border
import Element.Font
import Element.Input
import EmailAddress exposing (EmailAddress)
import Html
import Html.Attributes as Attr
import IdDict
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Route exposing (Route)
import String.Nonempty
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
        route : Route
        route =
            Url.Parser.parse Route.decode url
                |> Maybe.withDefault Route.CreateSurvey
                |> Debug.log "a"
    in
    case route of
        Route.CreateSurvey ->
            ( { key = key
              , state =
                    CreatingSurvey
                        { questions = Nonempty "" []
                        , submitState = NotSubmitted HasNotPressedSubmit
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
            String.split "," text
                |> List.filterMap
                    (\subtext ->
                        let
                            trimmed =
                                String.trim subtext
                        in
                        if trimmed == "" then
                            Nothing

                        else
                            Just ( trimmed, EmailAddress.fromString trimmed )
                    )

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
                    Err "Include at least one email address. Otherwise the survey won't be sent to anyone!"

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
                            ( { creatingSurvey | submitState = NotSubmitted HasPressedSubmit }
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
                                        , emailedTo =
                                            List.Nonempty.map
                                                (\email -> { email = email, result = SendingEmail })
                                                emailTo
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
                                , submitState = NotSubmitted HasNotPressedSubmit
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
                    let
                        showRemoveButton =
                            List.Nonempty.length creatingSurvey2.questions > 1
                    in
                    Element.column
                        [ contentWidth, Element.centerX, Element.padding 16, Element.spacing 32 ]
                        [ title "Create a survey"
                        , Element.column
                            [ Element.spacing 32, Element.width Element.fill ]
                            [ Element.column
                                [ Element.spacing 16, Element.width Element.fill ]
                                (List.indexedMap
                                    (editQuestionView showRemoveButton)
                                    (List.Nonempty.toList creatingSurvey2.questions)
                                )
                            , button buttonAttributes PressedAddQuestion (Element.text "Add new question")
                            ]
                        , Element.column
                            [ Element.spacing 4 ]
                            [ Element.Input.text
                                []
                                { onChange = TypedEmailTo
                                , text = creatingSurvey2.emailTo
                                , placeholder = Nothing
                                , label =
                                    Element.Input.labelAbove
                                        []
                                        (Element.paragraph
                                            []
                                            [ Element.text "List the people you want this survey emailed to. Separate each email with a comma (i.e. john.doe@example.com, bob-smith@bob.com, jane123@mail.com)"
                                            ]
                                        )
                                }
                            , case ( creatingSurvey2.submitState, validateEmails creatingSurvey2.emailTo ) of
                                ( NotSubmitted HasPressedSubmit, Err error ) ->
                                    Element.paragraph [ Element.Font.color (Element.rgb 1 0 0) ] [ Element.text error ]

                                _ ->
                                    Element.none
                            ]
                        , button
                            buttonAttributes
                            PressedCreateSurvey
                            (Element.text "Create survey")
                        ]
                        |> Element.map CreateSurveyMsg

                SurveyOverviewAdmin _ survey ->
                    Element.column
                        [ contentWidth, Element.centerX, Element.padding 16, Element.spacing 32 ]
                        [ title "Survey results"
                        , Element.column
                            [ Element.spacing 16 ]
                            (List.map questionResultView (List.Nonempty.toList survey.questions))
                        ]

                LoadingSurveyFailed loadSurveyError ->
                    Element.text ""
            )
        ]
    }


title : String -> Element msg
title text =
    Element.paragraph [ Element.Font.size 24, Element.Font.bold ] [ Element.text text ]


questionResultView : SurveyQuestion -> Element msg
questionResultView { question, answers } =
    Element.column
        [ Element.width Element.fill, Element.spacing 8 ]
        [ Element.paragraph [ Element.Font.bold ] [ Element.text question ]
        , if Dict.isEmpty answers then
            Element.paragraph
                [ Element.Font.italic, Element.Font.color (Element.rgb 0.3 0.3 0.3), Element.Font.size 16 ]
                [ Element.text "No one has answered yet" ]

          else
            List.map
                (\( emailAddress, answer ) ->
                    Element.row
                        [ Element.width Element.fill ]
                        [ Element.el [ Element.Font.bold, Element.Font.size 16 ] (Element.text (EmailAddress.toString emailAddress))
                        , Element.paragraph [ Element.Font.size 16 ] [ Element.text (String.Nonempty.toString answer) ]
                        ]
                )
                (Dict.toList answers)
                |> Element.column [ Element.spacing 16, Element.width Element.fill ]
        ]


buttonAttributes : List (Element.Attribute msg)
buttonAttributes =
    [ Element.width Element.fill
    , Element.Background.color (Element.rgb 0.9 0.9 0.9)
    , Element.padding 8
    , Element.Border.shadow { offset = ( 0, 0 ), size = 0, blur = 2, color = Element.rgba 0 0 0 0.4 }
    , Element.Border.rounded 4
    , Element.Font.center
    , Element.Font.bold
    ]


contentWidth : Element.Attribute msg
contentWidth =
    Element.width (Element.maximum 800 Element.fill)


editQuestionView : Bool -> Int -> String -> Element CreateSurveyMsg
editQuestionView showRemoveButton index text =
    Element.row
        [ Element.spacing 8, Element.width Element.fill ]
        [ Element.Input.multiline
            [ Element.width Element.fill ]
            { onChange = TypedQuestion index
            , text = text
            , placeholder = Nothing
            , label =
                Element.Input.labelAbove
                    [ Element.Font.bold ]
                    (Element.text ("Question " ++ String.fromInt (index + 1)))
            , spellcheck = True
            }
        , Element.row
            [ Element.moveDown 8 ]
            [ Element.row
                []
                [ button [ Element.paddingXY 12 8 ] (PressedMoveDownQuestion index) (Element.text "▼")
                , button [ Element.paddingXY 12 8 ] (PressedMoveUpQuestion index) (Element.text "▲")
                ]
            , if showRemoveButton then
                button
                    [ Element.Background.color (Element.rgb 1 0 0)
                    , Element.Font.color (Element.rgb 1 1 1)
                    , Element.paddingXY 12 8
                    ]
                    (PressedRemoveQuestion index)
                    (Element.text "×")

              else
                Element.none
            ]
        ]


button : List (Element.Attribute msg) -> msg -> Element msg -> Element msg
button attributes msg label =
    Element.Input.button attributes { onPress = Just msg, label = label }
