module Types exposing (..)

import Browser exposing (UrlRequest)
import Effect.Browser.Navigation
import EmailAddress exposing (EmailAddress)
import Id exposing (Id, SurveyId, UserToken)
import IdDict exposing (IdDict)
import String.Nonempty exposing (NonemptyString)
import Url exposing (Url)


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , state : FrontendState
    }


type FrontendState
    = LoadingSurvey (Id SurveyId) (Id UserToken)
    | AnsweringSurvey AnsweringSurvey2
    | SubmittedSurvey
    | CreatingSurvey CreatingSurvey2
    | SurveyOverviewAdmin (Id SurveyId) BackendSurvey
    | LoadingSurveyFailed LoadSurveyError


type alias AnsweringSurvey2 =
    { surveyId : Id SurveyId
    , answers : List { question : String, answer : String }
    , submitState : SubmitState
    }


type alias CreatingSurvey2 =
    { questions : List String
    , emailTo : String
    , submitState : SubmitState
    }


type SubmitState
    = NotSubmitted HasSubmitted
    | Submitting


type HasSubmitted
    = HasPressedSubmitted
    | HasNotPressedSubmitted


type alias BackendModel =
    { surveys : IdDict SurveyId BackendSurvey
    }


type alias BackendSurvey =
    { questions : List SurveyQuestion
    , emailedTo : IdDict UserToken EmailAddress
    , owner : Id UserToken
    }


type alias FrontendSurvey =
    { questions : List SurveyQuestion
    , emailedTo : IdDict UserToken EmailAddress
    }


type alias SurveyQuestion =
    { question : String, answers : IdDict EmailAddress NonemptyString }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | PressedSubmitSurvey
    | CreateSurveyMsg CreateSurveyMsg
    | TypedAnswer Int String


type CreateSurveyMsg
    = PressedCreateSurvey
    | PressedAddQuestion
    | PressedRemoveQuestion Int
    | PressedMoveUpQuestion Int
    | PressedMoveDownQuestion Int
    | TypedQuestion Int String
    | TypedEmailRecipients String


type ToBackend
    = SubmitSurveyRequest (List { answer : String })
    | CreateSurveyRequest (List { question : String }) (List EmailAddress)
    | LoadSurveyRequest (Id SurveyId) (Id UserToken)


type BackendMsg
    = NoOpBackendMsg


type LoadSurveyError
    = InvalidSurveyLink
    | SurveyAlreadySubmitted


type ToFrontend
    = SubmitSurveyResponse
    | CreateSurveyResponse (Id SurveyId) (Id UserToken)
    | LoadSurveyResponse (Id SurveyId) (Result LoadSurveyError (List { question : String }))
    | LoadSurveyAdminResponse (Result () FrontendSurvey)
