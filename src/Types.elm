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
import Survey exposing (BackendSurvey, EmailStatus, FrontendSurvey)
import SurveyName exposing (SurveyName)
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
    , emailAddress : EmailAddress
    , title : SurveyName
    , answers : Nonempty { question : String, answer : String }
    , submitState : SubmitState ()
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


type CreateSurveyMsg
    = PressedCreateSurvey
    | PressedAddQuestion
    | PressedRemoveQuestion Int
    | PressedMoveUpQuestion Int
    | PressedMoveDownQuestion Int
    | TypedQuestion Int String
    | TypedEmailTo String
    | TypedSurveyName String


type ToBackend
    = SubmitSurveyRequest (Id SurveyId) (Id UserToken) (Nonempty { answer : String })
    | CreateSurveyRequest SurveyName (Nonempty { question : String }) (Nonempty EmailAddress)
    | LoadSurveyRequest (Id SurveyId) (Id UserToken)


type BackendMsg
    = SurveyEmailSent (Id SurveyId) EmailAddress (Result Http.Error PostmarkSendResponse)


type LoadSurveyError
    = InvalidSurveyLink
    | SurveyAlreadySubmitted


type ToFrontend
    = SubmitSurveyResponse
    | CreateSurveyResponse (Id SurveyId) (Id UserToken) (Nonempty ( Id UserToken, { email : EmailAddress, emailStatus : EmailStatus } ))
    | LoadSurveyResponse
        (Result
            LoadSurveyError
            { surveyId : Id SurveyId
            , userToken : Id UserToken
            , emailAddress : EmailAddress
            , surveyName : SurveyName
            , questions : Nonempty { question : String }
            }
        )
    | LoadSurveyAdminResponse (Id SurveyId) (Result () FrontendSurvey)
