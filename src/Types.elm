module Types exposing (..)

import Browser exposing (UrlRequest)
import EmailAddress exposing (EmailAddress)
import IdDict exposing (IdDict)
import String.Nonempty exposing (NonemptyString)
import Url exposing (Url)


type FrontendModel
    = LoadingSurvey (Id SurveyId) (Id UserToken)
    | AnsweringSurvey (Id SurveyId) (List { question : String, answer : String })
    | SubmittedSurvey
    | CreatingSurvey (List String)
    | LoadingSurveyAdmin (Id SurveyId) (Id AdminToken)
    | SurveyOverviewAdmin (Id SurveyId) BackendSurvey


type Id a
    = Id String


type UserToken
    = UserToken Never


type AdminToken
    = AdminToken Never


type SurveyId
    = SurveyId Never


type alias BackendModel =
    { surveys : IdDict SurveyId BackendSurvey
    }


type alias BackendSurvey =
    { questions : List SurveyQuestion
    , emailedTo : IdDict UserToken EmailAddress
    }


type alias FrontendSurvey =
    { questions : List SurveyQuestion
    , emailedTo : IdDict UserToken EmailAddress
    }


type alias SurveyQuestion =
    { question : String, answers : IdDict EmailAddress NonemptyString }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedSubmitSurvey
    | PressedCreateSurvey
    | PressedAddQuestion
    | TypedAnswer Int String
    | TypedQuestion Int String
    | TypedEmailRecipients String


type ToBackend
    = SubmitSurveyRequest (List { answer : String })
    | CreateSurveyRequest (List { question : String })
    | LoadSurveyRequest (Id SurveyId) (Id UserToken)
    | LoadSurveyAdminRequest (Id SurveyId) (Id AdminToken)


type BackendMsg
    = NoOpBackendMsg


type LoadSurveyError
    = InvalidSurveyLink
    | SurveyAlreadySubmitted


type ToFrontend
    = SubmitSurveyResponse
    | CreateSurveyResponse (Id SurveyId) (Id AdminToken)
    | LoadSurveyResponse (Result LoadSurveyError (List { question : String }))
    | LoadSurveyAdminResponse (Result () FrontendSurvey)
