module Types exposing (..)

import AssocList exposing (Dict)
import Browser exposing (UrlRequest)
import Effect.Browser.Navigation
import Effect.Http as Http
import Effect.Lamdera exposing (ClientId, SessionId)
import EmailAddress exposing (EmailAddress)
import Id exposing (Id, SurveyId, UserToken)
import IdDict exposing (IdDict)
import List.Nonempty exposing (Nonempty)
import Postmark exposing (PostmarkSendResponse)
import String.Nonempty exposing (NonemptyString)
import Survey exposing (BackendSurvey, EmailStatus, FrontendSurvey)
import SurveyName exposing (SurveyName)
import Time
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
    | SurveyOverviewAdmin SurveyOverview
    | LoadingSurveyFailed LoadSurveyError
    | PrivacyPage


type alias SurveyOverview =
    { surveyId : Id SurveyId
    , survey : FrontendSurvey
    , addMoreEmailTo : String
    , sendingMoreEmails : Bool
    }


type alias AnsweringSurvey2 =
    { surveyId : Id SurveyId
    , userToken : Id UserToken
    , emailAddress : EmailAddress
    , title : SurveyName
    , answers : Nonempty { question : String, answer : String }
    , submitState : SubmitState ()
    , creationTime : Time.Posix
    }


type alias CreatingSurvey2 =
    { surveyName : String
    , questions : Nonempty String
    , emailTo : String
    , submitState : SubmitState { surveyName : SurveyName, questions : Nonempty String, emailTo : Nonempty EmailAddress }
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
    , surveyIdCounter : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | PressedSubmitSurvey
    | CreateSurveyMsg CreateSurveyMsg
    | TypedAnswer Int String
    | SurveyOverviewMsg SurveyOverviewMsg


type CreateSurveyMsg
    = PressedCreateSurvey
    | PressedAddQuestion
    | PressedRemoveQuestion Int
    | PressedMoveUpQuestion Int
    | PressedMoveDownQuestion Int
    | TypedQuestion Int String
    | TypedEmailTo String
    | TypedSurveyName String


type SurveyOverviewMsg
    = TypedAddMoreEmailTo String
    | SubmitMoreEmailTo


type ToBackend
    = SubmitSurveyRequest (Id SurveyId) (Id UserToken) (Nonempty { answer : String })
    | CreateSurveyRequest SurveyName (Nonempty { question : String }) (Nonempty EmailAddress)
    | LoadSurveyRequest (Id SurveyId) (Id UserToken)
    | SendMoreEmailsRequest (Id SurveyId) (Id UserToken) (Nonempty EmailAddress)


type BackendMsg
    = SurveyEmailSent (Id SurveyId) EmailAddress (Result Postmark.Error ())
    | GotTime Time.Posix SessionId ClientId ToBackend
    | HourElapsed Time.Posix


type LoadSurveyError
    = InvalidSurveyLink
    | SurveyAlreadySubmitted


type ToFrontend
    = SubmitSurveyResponse
    | CreateSurveyResponse (Id SurveyId) (Id UserToken) (Nonempty ( Id UserToken, { email : EmailAddress, emailStatus : EmailStatus } )) Time.Posix
    | LoadSurveyResponse
        (Result
            LoadSurveyError
            { surveyId : Id SurveyId
            , userToken : Id UserToken
            , emailAddress : EmailAddress
            , surveyName : SurveyName
            , questions : Nonempty { question : String }
            , creationTime : Time.Posix
            }
        )
    | LoadSurveyAdminResponse (Id SurveyId) (Result () FrontendSurvey)
    | SendMoreEmailsResponse (Nonempty ( Id UserToken, { email : EmailAddress, emailStatus : EmailStatus } ))
