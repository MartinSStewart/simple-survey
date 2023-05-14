module Evergreen.V2.Types exposing (..)

import Browser
import Effect.Browser.Navigation
import Effect.Lamdera
import Evergreen.V2.EmailAddress
import Evergreen.V2.Id
import Evergreen.V2.IdDict
import Evergreen.V2.Postmark
import Evergreen.V2.Survey
import Evergreen.V2.SurveyName
import List.Nonempty
import Time
import Url


type HasSubmitted
    = HasPressedSubmit
    | HasNotPressedSubmit


type SubmitState a
    = NotSubmitted HasSubmitted
    | Submitting a


type alias AnsweringSurvey2 =
    { surveyId : Evergreen.V2.Id.Id Evergreen.V2.Id.SurveyId
    , userToken : Evergreen.V2.Id.Id Evergreen.V2.Id.UserToken
    , emailAddress : Evergreen.V2.EmailAddress.EmailAddress
    , title : Evergreen.V2.SurveyName.SurveyName
    , answers :
        List.Nonempty.Nonempty
            { question : String
            , answer : String
            }
    , submitState : SubmitState ()
    , creationTime : Time.Posix
    }


type alias CreatingSurvey2 =
    { surveyName : String
    , questions : List.Nonempty.Nonempty String
    , emailTo : String
    , submitState :
        SubmitState
            { surveyName : Evergreen.V2.SurveyName.SurveyName
            , questions : List.Nonempty.Nonempty String
            , emailTo : List.Nonempty.Nonempty Evergreen.V2.EmailAddress.EmailAddress
            }
    }


type LoadSurveyError
    = InvalidSurveyLink
    | SurveyAlreadySubmitted


type FrontendState
    = LoadingSurvey (Evergreen.V2.Id.Id Evergreen.V2.Id.SurveyId) (Evergreen.V2.Id.Id Evergreen.V2.Id.UserToken)
    | AnsweringSurvey AnsweringSurvey2
    | SubmittedSurvey
    | CreatingSurvey CreatingSurvey2
    | SurveyOverviewAdmin (Evergreen.V2.Id.Id Evergreen.V2.Id.SurveyId) Evergreen.V2.Survey.FrontendSurvey
    | LoadingSurveyFailed LoadSurveyError
    | PrivacyPage


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , state : FrontendState
    }


type alias BackendModel =
    { surveys : Evergreen.V2.IdDict.IdDict Evergreen.V2.Id.SurveyId Evergreen.V2.Survey.BackendSurvey
    , secretCounter : Int
    , surveyIdCounter : Int
    }


type CreateSurveyMsg
    = PressedCreateSurvey
    | PressedAddQuestion
    | PressedRemoveQuestion Int
    | PressedMoveUpQuestion Int
    | PressedMoveDownQuestion Int
    | TypedQuestion Int String
    | TypedEmailTo String
    | TypedSurveyName String


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PressedSubmitSurvey
    | CreateSurveyMsg CreateSurveyMsg
    | TypedAnswer Int String


type ToBackend
    = SubmitSurveyRequest
        (Evergreen.V2.Id.Id Evergreen.V2.Id.SurveyId)
        (Evergreen.V2.Id.Id Evergreen.V2.Id.UserToken)
        (List.Nonempty.Nonempty
            { answer : String
            }
        )
    | CreateSurveyRequest
        Evergreen.V2.SurveyName.SurveyName
        (List.Nonempty.Nonempty
            { question : String
            }
        )
        (List.Nonempty.Nonempty Evergreen.V2.EmailAddress.EmailAddress)
    | LoadSurveyRequest (Evergreen.V2.Id.Id Evergreen.V2.Id.SurveyId) (Evergreen.V2.Id.Id Evergreen.V2.Id.UserToken)


type BackendMsg
    = SurveyEmailSent (Evergreen.V2.Id.Id Evergreen.V2.Id.SurveyId) Evergreen.V2.EmailAddress.EmailAddress (Result Evergreen.V2.Postmark.Error ())
    | GotTime Time.Posix Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend
    | HourElapsed Time.Posix


type ToFrontend
    = SubmitSurveyResponse
    | CreateSurveyResponse
        (Evergreen.V2.Id.Id Evergreen.V2.Id.SurveyId)
        (Evergreen.V2.Id.Id Evergreen.V2.Id.UserToken)
        (List.Nonempty.Nonempty
            ( Evergreen.V2.Id.Id Evergreen.V2.Id.UserToken
            , { email : Evergreen.V2.EmailAddress.EmailAddress
              , emailStatus : Evergreen.V2.Survey.EmailStatus
              }
            )
        )
        Time.Posix
    | LoadSurveyResponse
        (Result
            LoadSurveyError
            { surveyId : Evergreen.V2.Id.Id Evergreen.V2.Id.SurveyId
            , userToken : Evergreen.V2.Id.Id Evergreen.V2.Id.UserToken
            , emailAddress : Evergreen.V2.EmailAddress.EmailAddress
            , surveyName : Evergreen.V2.SurveyName.SurveyName
            , questions :
                List.Nonempty.Nonempty
                    { question : String
                    }
            , creationTime : Time.Posix
            }
        )
    | LoadSurveyAdminResponse (Evergreen.V2.Id.Id Evergreen.V2.Id.SurveyId) (Result () Evergreen.V2.Survey.FrontendSurvey)
