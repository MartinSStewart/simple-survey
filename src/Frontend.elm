module Frontend exposing (..)

import AssocList as Dict
import Browser exposing (UrlRequest(..))
import Duration
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
import Env
import Html
import Html.Attributes
import Id exposing (Id, UserToken)
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Route exposing (Route)
import String.Nonempty
import Survey exposing (EmailStatus(..), SurveyQuestion)
import SurveyName exposing (Error(..), SurveyName)
import Time exposing (Month(..))
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

        ( newState, cmd ) =
            routeChanged
                route
                (CreatingSurvey
                    { surveyName = ""
                    , questions = Nonempty "" []
                    , submitState = NotSubmitted HasNotPressedSubmit
                    , emailTo = ""
                    }
                )
    in
    ( { key = key, state = newState }, cmd )


routeChanged : Route -> FrontendState -> ( FrontendState, Command FrontendOnly ToBackend FrontendMsg )
routeChanged route state =
    case route of
        Route.CreateSurvey ->
            case state of
                CreatingSurvey _ ->
                    ( state, Command.none )

                _ ->
                    ( CreatingSurvey
                        { surveyName = ""
                        , questions = Nonempty "" []
                        , submitState = NotSubmitted HasNotPressedSubmit
                        , emailTo = ""
                        }
                    , Command.none
                    )

        Route.ViewSurvey surveyId userToken ->
            ( LoadingSurvey surveyId userToken
            , LoadSurveyRequest surveyId userToken |> Effect.Lamdera.sendToBackend
            )

        Route.Privacy ->
            ( PrivacyPage, Command.none )


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
            let
                route : Route
                route =
                    Url.Parser.parse Route.decode url
                        |> Maybe.withDefault Route.CreateSurvey

                ( state, cmd ) =
                    routeChanged route model.state
            in
            ( { model | state = state }, cmd )

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


validateSurveyName : String -> Result String SurveyName
validateSurveyName text =
    case SurveyName.fromString text of
        Ok ok ->
            Ok ok

        Err SurveyNameTooLong ->
            Err "Pick a shorter name"

        Err SurveyNameTooShort ->
            Err "Can't be empty"


updateCreateSurvey : CreateSurveyMsg -> CreatingSurvey2 -> ( CreatingSurvey2, Command FrontendOnly ToBackend FrontendMsg )
updateCreateSurvey msg creatingSurvey =
    case msg of
        PressedCreateSurvey ->
            case creatingSurvey.submitState of
                NotSubmitted _ ->
                    case ( validateEmails creatingSurvey.emailTo, validateSurveyName creatingSurvey.surveyName ) of
                        ( Ok emails, Ok surveyName ) ->
                            ( { creatingSurvey
                                | submitState =
                                    Submitting
                                        { surveyName = surveyName
                                        , questions = creatingSurvey.questions
                                        , emailTo = emails
                                        }
                              }
                            , CreateSurveyRequest
                                surveyName
                                (List.Nonempty.map (\question -> { question = question }) creatingSurvey.questions)
                                emails
                                |> Effect.Lamdera.sendToBackend
                            )

                        _ ->
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

        TypedSurveyName text ->
            ( { creatingSurvey | surveyName = text }, Command.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Command FrontendOnly toMsg FrontendMsg )
updateFromBackend msg model =
    case msg of
        SubmitSurveyResponse ->
            case model.state of
                AnsweringSurvey _ ->
                    ( { model | state = SubmittedSurvey }, Command.none )

                _ ->
                    ( model, Command.none )

        CreateSurveyResponse surveyId userToken emailedTo creationTime ->
            case model.state of
                CreatingSurvey { submitState } ->
                    case submitState of
                        Submitting { surveyName, questions, emailTo } ->
                            ( { model
                                | state =
                                    SurveyOverviewAdmin
                                        surveyId
                                        { title = surveyName
                                        , questions =
                                            List.Nonempty.map
                                                (\question -> { question = question, answers = Dict.empty })
                                                questions
                                        , emailedTo = emailedTo
                                        , owner = userToken
                                        , creationTime = creationTime
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

        LoadSurveyResponse result ->
            ( case result of
                Ok { surveyId, userToken, emailAddress, surveyName, questions, creationTime } ->
                    { model
                        | state =
                            AnsweringSurvey
                                { title = surveyName
                                , surveyId = surveyId
                                , userToken = userToken
                                , emailAddress = emailAddress
                                , answers =
                                    List.Nonempty.map
                                        (\{ question } -> { question = question, answer = "" })
                                        questions
                                , submitState = NotSubmitted HasNotPressedSubmit
                                , creationTime = creationTime
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


unansweredQuestions : Nonempty { a | answer : String } -> Int
unansweredQuestions list =
    List.count (\{ answer } -> String.trim answer == "") (List.Nonempty.toList list)


answerQuestionView : Int -> { question : String, answer : String } -> Element FrontendMsg
answerQuestionView index { question, answer } =
    Element.Input.multiline
        []
        { onChange = TypedAnswer index
        , text = answer
        , placeholder = Nothing
        , label = Element.Input.labelAbove [ Element.Font.bold ] (whiteSpaceParagraph question)
        , spellcheck = True
        }


pageView state =
    case state of
        LoadingSurvey _ _ ->
            Element.text "Loading..."

        AnsweringSurvey answeringSurvey2 ->
            Element.column
                [ contentWidth, Element.centerX, Element.padding 16, Element.spacing 32 ]
                [ title (SurveyName.toString answeringSurvey2.title)
                , Element.paragraph
                    []
                    [ Element.text "Answering as "
                    , Element.el
                        [ Element.Font.bold ]
                        (Element.text (EmailAddress.toString answeringSurvey2.emailAddress))
                    ]
                , List.Nonempty.toList answeringSurvey2.answers
                    |> List.indexedMap answerQuestionView
                    |> Element.column [ Element.spacing 16, Element.width Element.fill, Element.Border.width 1, Element.paddingXY 16 24 ]
                , button
                    buttonAttributes
                    PressedSubmitSurvey
                    ((case answeringSurvey2.submitState of
                        NotSubmitted _ ->
                            "Submit survey"
                                ++ (case unansweredQuestions answeringSurvey2.answers of
                                        0 ->
                                            ""

                                        1 ->
                                            " (1 question skipped)"

                                        count ->
                                            " (" ++ String.fromInt count ++ " questions skipped)"
                                   )

                        Submitting _ ->
                            "Submitting"
                     )
                        |> Element.text
                    )
                , "Survey created on "
                    ++ timeToDateString True answeringSurvey2.creationTime
                    |> Element.text
                    |> Element.el [ Element.Font.size 16 ]
                ]

        SubmittedSurvey ->
            Element.el
                [ Element.centerX, Element.centerY, Element.padding 16, Element.Font.size 26 ]
                (Element.paragraph
                    []
                    [ Element.text "Survey successfully submitted. Thanks!" ]
                )

        CreatingSurvey creatingSurvey2 ->
            let
                showRemoveButton =
                    List.Nonempty.length creatingSurvey2.questions > 1

                showError result =
                    case ( creatingSurvey2.submitState, result ) of
                        ( NotSubmitted HasPressedSubmit, Err error ) ->
                            Element.paragraph
                                [ Element.Font.color (Element.rgb 1 0 0)
                                , Element.Font.size 16
                                ]
                                [ Element.text error ]

                        _ ->
                            Element.none
            in
            Element.column
                [ contentWidth, Element.centerX, Element.padding 16, Element.spacing 32 ]
                [ title "Create a survey"
                , Element.column
                    [ Element.width Element.fill, Element.spacing 4 ]
                    [ Element.Input.text
                        []
                        { onChange = TypedSurveyName
                        , text = creatingSurvey2.surveyName
                        , placeholder = Nothing
                        , label = Element.Input.labelAbove [ Element.Font.bold ] (Element.text "Survey name")
                        }
                    , showError (validateSurveyName creatingSurvey2.surveyName)
                    ]
                , Element.column
                    [ Element.spacing 32
                    , Element.width Element.fill
                    , Element.Border.width 1
                    , Element.paddingEach { left = 16, right = 16, top = 16, bottom = 24 }
                    ]
                    [ Element.column
                        [ Element.spacing 16, Element.width Element.fill ]
                        (List.indexedMap
                            (editQuestionView showRemoveButton)
                            (List.Nonempty.toList creatingSurvey2.questions)
                        )
                    , button buttonAttributes PressedAddQuestion (Element.text "Add new question")
                    ]
                , Element.column
                    [ Element.spacing 4, Element.width Element.fill ]
                    [ Element.Input.text
                        []
                        { onChange = TypedEmailTo
                        , text = creatingSurvey2.emailTo
                        , placeholder =
                            Element.text "johnz@example.com, bob@bob.com, jane123@mail.com"
                                |> Element.Input.placeholder []
                                |> Just
                        , label =
                            Element.Input.labelAbove
                                []
                                (Element.paragraph
                                    [ Element.Font.bold ]
                                    [ Element.text "List the people you want this survey emailed to. Comma separate each email."
                                    ]
                                )
                        }
                    , showError (validateEmails creatingSurvey2.emailTo)
                    ]
                , button
                    buttonAttributes
                    PressedCreateSurvey
                    (Element.text "Create survey")
                ]
                |> Element.map CreateSurveyMsg

        SurveyOverviewAdmin surveyId survey ->
            let
                successfulEmails : List (Element msg)
                successfulEmails =
                    List.filterMap
                        (\( _, { email, emailStatus } ) ->
                            case emailStatus of
                                EmailSuccess ->
                                    EmailAddress.toString email
                                        |> Element.text
                                        |> Element.el [ Element.Font.bold ]
                                        |> Just

                                EmailError _ ->
                                    if Survey.hasSubmitted email survey then
                                        EmailAddress.toString email
                                            |> Element.text
                                            |> Element.el [ Element.Font.bold ]
                                            |> Just

                                    else
                                        Nothing

                                SendingEmail ->
                                    Nothing
                        )
                        (List.Nonempty.toList survey.emailedTo)

                failedEmails : List ( EmailAddress, Id UserToken )
                failedEmails =
                    List.filterMap
                        (\( userToken, { email, emailStatus } ) ->
                            case emailStatus of
                                EmailError _ ->
                                    if Survey.hasSubmitted email survey then
                                        Nothing

                                    else
                                        Just ( email, userToken )

                                _ ->
                                    Nothing
                        )
                        (List.Nonempty.toList survey.emailedTo)

                adminUrl =
                    Env.domain ++ Route.encode (Route.ViewSurvey surveyId survey.owner)
            in
            Element.column
                [ contentWidth, Element.centerX, Element.padding 16, Element.spacing 32 ]
                [ title ("Survey results for " ++ SurveyName.toString survey.title)
                , Element.paragraph []
                    [ Element.el [ Element.Font.bold ] (Element.text "Important!")
                    , Element.text " Save this link so you can return to this page later: "
                    , Element.link
                        [ Element.Font.color (Element.rgb 0.2 0.3 1), Element.Font.underline ]
                        { url = adminUrl, label = Element.text adminUrl }
                    ]
                , Element.column
                    [ Element.spacing 24
                    , Element.Border.width 1
                    , Element.padding 16
                    , Element.width Element.fill
                    ]
                    (List.map questionResultView (List.Nonempty.toList survey.questions))
                , if List.isEmpty successfulEmails then
                    Element.none

                  else
                    Element.paragraph
                        [ Element.Font.size 16 ]
                        (Element.text "Invites sent successfully to "
                            :: List.intersperse (Element.text ", ") successfulEmails
                        )
                , if List.isEmpty failedEmails then
                    Element.none

                  else
                    Element.column
                        [ Element.Font.color (Element.rgb 1 0 0), Element.spacing 16, Element.width Element.fill ]
                        [ Element.paragraph [] [ Element.text "Failed to email invites to the following people:" ]
                        , Element.table
                            [ Element.Border.width 1, Element.padding 8, Element.width Element.fill ]
                            { data = failedEmails
                            , columns =
                                [ { header = Element.el [ Element.paddingXY 12 8, Element.Font.bold ] (Element.text "Email")
                                  , width = Element.shrink
                                  , view =
                                        \( email, _ ) ->
                                            EmailAddress.toString email
                                                |> Element.text
                                                |> Element.el [ Element.paddingXY 12 8, Element.Font.size 16 ]
                                  }
                                , { header =
                                        Element.paragraph
                                            [ Element.paddingXY 12 8, Element.Font.bold ]
                                            [ Element.text "Invite link (you'll\u{00A0}need\u{00A0}to\u{00A0}manually\u{00A0}send\u{00A0}these)" ]
                                  , width = Element.fill
                                  , view =
                                        \( _, userToken ) ->
                                            (Env.domain ++ Route.encode (Route.ViewSurvey surveyId userToken))
                                                |> Element.text
                                                |> Element.el [ Element.paddingXY 12 8, Element.Font.size 16 ]
                                  }
                                ]
                            }
                        ]
                , Element.paragraph
                    [ Element.Font.size 16 ]
                    [ "Survey created on "
                        ++ timeToDateString True survey.creationTime
                        ++ ". For privacy reasons, this survey will automatically be deleted on "
                        ++ timeToDateString False (Duration.addTo survey.creationTime (Duration.days 90))
                        ++ " (90 days)."
                        |> Element.text
                    ]
                ]

        LoadingSurveyFailed error ->
            Element.column
                [ Element.spacing 16, Element.centerX, Element.centerY, Element.padding 16 ]
                [ Element.paragraph
                    [ Element.Font.size 26 ]
                    [ (case error of
                        InvalidSurveyLink ->
                            "Survey doesn't exist or you don't have access to it."

                        SurveyAlreadySubmitted ->
                            "Your survey has already been submitted."
                      )
                        |> Element.text
                    ]
                ]

        PrivacyPage ->
            Element.column
                [ contentWidth, Element.centerX, Element.padding 16, Element.spacing 32 ]
                [ title "Privacy"
                , Element.paragraph
                    []
                    [ Element.text "The goal of this website is to allow people to create surveys without worrying that user data is being sent to 3rd parties or deal with the hassle of account creation."
                    ]
                , Element.paragraph [] [ Element.text "Survey questions, responses, and emails addresses are only visible to the person who created the survey and the website admin*. Surveys are also automatically deleted after 90 days." ]
                , Element.paragraph []
                    [ Element.text "Additionally "
                    , Element.newTabLink
                        [ Element.Font.color (Element.rgb 0.2 0.3 1), Element.Font.underline ]
                        { url = "https://github.com/MartinSStewart/simple-survey"
                        , label = Element.text "this website is open source"
                        }
                    , Element.text " so you can verify that these claims are true (bug reports, feature suggestions, and pull requests are also welcome)."
                    ]
                , Element.paragraph
                    [ Element.Font.size 16 ]
                    [ Element.text "* There isn't a way for the admin to directly view surveys but it's still possible for them to download the server data and manually view it." ]
                ]


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout
            []
            (Element.column
                [ Element.width Element.fill, Element.height Element.fill, Element.spacing 32 ]
                [ pageView model.state
                , Element.el
                    [ Element.Font.color (Element.rgb 1 1 1)
                    , Element.Background.color (Element.rgb 0 0.3 0)
                    , Element.Font.bold
                    , Element.width Element.fill
                    , Element.alignBottom
                    ]
                    (Element.row [ contentWidth, Element.centerX, Element.paddingXY 16 0, Element.spacing 32 ]
                        [ Element.link
                            [ Element.padding 8 ]
                            { url = Route.encode Route.CreateSurvey, label = Element.text "Create Survey" }
                        , Element.link
                            [ Element.padding 8 ]
                            { url = Route.encode Route.Privacy, label = Element.text "Privacy" }
                        ]
                    )
                ]
            )
        ]
    }


title : String -> Element msg
title text =
    Element.paragraph [ Element.Font.size 26, Element.Font.bold ] [ Element.text text ]


questionResultView : SurveyQuestion -> Element msg
questionResultView { question, answers } =
    Element.column
        [ Element.width Element.fill, Element.spacing 8 ]
        [ whiteSpaceParagraph question |> Element.el [ Element.Font.bold ]
        , if Dict.isEmpty answers then
            Element.paragraph
                [ Element.Font.italic, Element.Font.color (Element.rgb 0.3 0.3 0.3), Element.Font.size 16 ]
                [ Element.text "No one has answered yet" ]

          else
            List.map
                (\( emailAddress, answer ) ->
                    Element.row
                        [ Element.width Element.fill, Element.spacing 16, Element.Font.size 16 ]
                        [ Element.el [ Element.Font.bold ] (Element.text (EmailAddress.toString emailAddress))
                        , String.Nonempty.toString answer
                            |> whiteSpaceParagraph
                            |> Element.el [ Element.width Element.fill ]
                        ]
                )
                (Dict.toList answers)
                |> Element.column [ Element.spacing 16, Element.width Element.fill ]
        ]


whiteSpaceParagraph : String -> Element msg
whiteSpaceParagraph text =
    Html.div
        [ Html.Attributes.style "white-space" "pre-wrap", Html.Attributes.style "line-height" "120%" ]
        [ Html.text text ]
        |> Element.html


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
        , if showRemoveButton then
            Element.row
                [ Element.moveDown 26, Element.alignTop, Element.spacing 4 ]
                [ Element.row
                    []
                    [ button
                        [ Element.paddingXY 12 12
                        , Element.Background.color (Element.rgb 0.9 0.9 0.9)
                        , Element.Border.roundEach { topLeft = 4, topRight = 0, bottomLeft = 4, bottomRight = 0 }
                        ]
                        (PressedMoveDownQuestion index)
                        (Element.text "▼")
                    , button
                        [ Element.paddingXY 12 12
                        , Element.Background.color (Element.rgb 0.9 0.9 0.9)
                        , Element.Border.roundEach { topLeft = 0, topRight = 4, bottomLeft = 0, bottomRight = 4 }
                        ]
                        (PressedMoveUpQuestion index)
                        (Element.text "▲")
                    ]
                , button
                    [ Element.Background.color (Element.rgb 1 0 0)
                    , Element.Font.color (Element.rgb 1 1 1)
                    , Element.paddingXY 12 12
                    , Element.Border.rounded 4
                    ]
                    (PressedRemoveQuestion index)
                    (Element.el [ Element.moveUp 1 ] (Element.text "🗙"))
                ]

          else
            Element.none
        ]


button : List (Element.Attribute msg) -> msg -> Element msg -> Element msg
button attributes msg label =
    Element.Input.button attributes { onPress = Just msg, label = label }


timeToDateString : Bool -> Time.Posix -> String
timeToDateString includeYear time =
    let
        year =
            Time.toYear Time.utc time |> String.fromInt

        month =
            Time.toMonth Time.utc time |> monthToName

        day =
            Time.toDay Time.utc time |> dayToText
    in
    if includeYear then
        month ++ " " ++ day ++ ", " ++ year

    else
        month ++ " " ++ day


dayToText : Int -> String
dayToText day =
    case day of
        1 ->
            "1st"

        2 ->
            "2nd"

        3 ->
            "3rd"

        21 ->
            "21st"

        22 ->
            "22nd"

        23 ->
            "23rd"

        31 ->
            "31st"

        _ ->
            String.fromInt day ++ "th"


monthToName : Month -> String
monthToName m =
    case m of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"
