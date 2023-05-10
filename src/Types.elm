module Types exposing (..)

import AssocList exposing (Dict)
import Browser exposing (UrlRequest)
import Effect.Browser.Navigation
import Effect.Http as Http
import EmailAddress exposing (EmailAddress)
import Id exposing (Id, SurveyId, UserToken)
import IdDict exposing (IdDict)
import List.Nonempty exposing (Nonempty)
import Postmark exposing (PostmarkSendResponse)
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
    | SurveyOverviewAdmin (Id SurveyId) FrontendSurvey
    | LoadingSurveyFailed LoadSurveyError


type alias AnsweringSurvey2 =
    { surveyId : Id SurveyId
    , userToken : Id UserToken
    , answers : Nonempty { question : String, answer : String }
    , submitState : SubmitState ()
    }


type alias CreatingSurvey2 =
    { questions : Nonempty String
    , emailTo : String
    , submitState : SubmitState { questions : Nonempty String, emailTo : Nonempty EmailAddress }
    }


type SubmitState a
    = NotSubmitted HasSubmitted
    | Submitting a


type HasSubmitted
    = HasPressedSubmit
    | HasNotPressedSubmit


type alias BackendModel =
    { surveys : IdDict SurveyId BackendSurvey
    , secretCounter : Int
    }


type alias BackendSurvey =
    { questions : Nonempty SurveyQuestion
    , emailedTo : Nonempty ( Id UserToken, { email : EmailAddress, result : EmailStatus } )
    , owner : Id UserToken
    }


type alias FrontendSurvey =
    { questions : Nonempty SurveyQuestion
    , emailedTo : Nonempty { email : EmailAddress, result : EmailStatus }
    }


type EmailStatus
    = SendingEmail
    | EmailError Http.Error
    | EmailSuccess PostmarkSendResponse


type alias SurveyQuestion =
    { question : String, answers : Dict EmailAddress NonemptyString }


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
    | TypedEmailTo String


type ToBackend
    = SubmitSurveyRequest (Id SurveyId) (Id UserToken) (Nonempty { answer : String })
    | CreateSurveyRequest (Nonempty { question : String }) (Nonempty EmailAddress)
    | LoadSurveyRequest (Id SurveyId) (Id UserToken)


type BackendMsg
    = NoOpBackendMsg


type LoadSurveyError
    = InvalidSurveyLink
    | SurveyAlreadySubmitted


type ToFrontend
    = SubmitSurveyResponse
    | CreateSurveyResponse (Id SurveyId) (Id UserToken)
    | LoadSurveyResponse (Id SurveyId) (Id UserToken) (Result LoadSurveyError (Nonempty { question : String }))
    | LoadSurveyAdminResponse (Id SurveyId) (Result () FrontendSurvey)
