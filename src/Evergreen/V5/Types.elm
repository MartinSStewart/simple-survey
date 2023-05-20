module Evergreen.V5.Types exposing (..)

import Browser
import Effect.Browser.Navigation
import Effect.Lamdera
import Evergreen.V5.EmailAddress
import Evergreen.V5.Id
import Evergreen.V5.IdDict
import Evergreen.V5.Postmark
import Evergreen.V5.Survey
import Evergreen.V5.SurveyName
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
    { surveyId : Evergreen.V5.Id.Id Evergreen.V5.Id.SurveyId
    , userToken : Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken
    , emailAddress : Evergreen.V5.EmailAddress.EmailAddress
    , title : Evergreen.V5.SurveyName.SurveyName
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
            { surveyName : Evergreen.V5.SurveyName.SurveyName
            , questions : List.Nonempty.Nonempty String
            , emailTo : List.Nonempty.Nonempty Evergreen.V5.EmailAddress.EmailAddress
            }
    }


type alias SurveyOverview =
    { surveyId : Evergreen.V5.Id.Id Evergreen.V5.Id.SurveyId
    , survey : Evergreen.V5.Survey.FrontendSurvey
    , addMoreEmailTo : String
    , sendingMoreEmails : Bool
    }


type LoadSurveyError
    = InvalidSurveyLink
    | SurveyAlreadySubmitted


type FrontendState
    = LoadingSurvey (Evergreen.V5.Id.Id Evergreen.V5.Id.SurveyId) (Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken)
    | AnsweringSurvey AnsweringSurvey2
    | SubmittedSurvey
    | CreatingSurvey CreatingSurvey2
    | SurveyOverviewAdmin SurveyOverview
    | LoadingSurveyFailed LoadSurveyError
    | PrivacyPage


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , state : FrontendState
    }


type alias BackendModel =
    { surveys : Evergreen.V5.IdDict.IdDict Evergreen.V5.Id.SurveyId Evergreen.V5.Survey.BackendSurvey
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


type SurveyOverviewMsg
    = TypedAddMoreEmailTo String
    | SubmitMoreEmailTo


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PressedSubmitSurvey
    | CreateSurveyMsg CreateSurveyMsg
    | TypedAnswer Int String
    | SurveyOverviewMsg SurveyOverviewMsg


type ToBackend
    = SubmitSurveyRequest
        (Evergreen.V5.Id.Id Evergreen.V5.Id.SurveyId)
        (Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken)
        (List.Nonempty.Nonempty
            { answer : String
            }
        )
    | CreateSurveyRequest
        Evergreen.V5.SurveyName.SurveyName
        (List.Nonempty.Nonempty
            { question : String
            }
        )
        (List.Nonempty.Nonempty Evergreen.V5.EmailAddress.EmailAddress)
    | LoadSurveyRequest (Evergreen.V5.Id.Id Evergreen.V5.Id.SurveyId) (Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken)
    | SendMoreEmailsRequest (Evergreen.V5.Id.Id Evergreen.V5.Id.SurveyId) (Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken) (List.Nonempty.Nonempty Evergreen.V5.EmailAddress.EmailAddress)


type BackendMsg
    = SurveyEmailSent (Evergreen.V5.Id.Id Evergreen.V5.Id.SurveyId) Evergreen.V5.EmailAddress.EmailAddress (Result Evergreen.V5.Postmark.Error ())
    | GotTime Time.Posix Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend
    | HourElapsed Time.Posix


type ToFrontend
    = SubmitSurveyResponse
    | CreateSurveyResponse
        (Evergreen.V5.Id.Id Evergreen.V5.Id.SurveyId)
        (Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken)
        (List.Nonempty.Nonempty
            ( Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken
            , { email : Evergreen.V5.EmailAddress.EmailAddress
              , emailStatus : Evergreen.V5.Survey.EmailStatus
              }
            )
        )
        Time.Posix
    | LoadSurveyResponse
        (Result
            LoadSurveyError
            { surveyId : Evergreen.V5.Id.Id Evergreen.V5.Id.SurveyId
            , userToken : Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken
            , emailAddress : Evergreen.V5.EmailAddress.EmailAddress
            , surveyName : Evergreen.V5.SurveyName.SurveyName
            , questions :
                List.Nonempty.Nonempty
                    { question : String
                    }
            , creationTime : Time.Posix
            }
        )
    | LoadSurveyAdminResponse (Evergreen.V5.Id.Id Evergreen.V5.Id.SurveyId) (Result () Evergreen.V5.Survey.FrontendSurvey)
    | SendMoreEmailsResponse
        (List.Nonempty.Nonempty
            ( Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken
            , { email : Evergreen.V5.EmailAddress.EmailAddress
              , emailStatus : Evergreen.V5.Survey.EmailStatus
              }
            )
        )
