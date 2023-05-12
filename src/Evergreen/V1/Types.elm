module Evergreen.V1.Types exposing (..)

import AssocList
import Browser
import Effect.Browser.Navigation
import Effect.Http
import Evergreen.V1.EmailAddress
import Evergreen.V1.Id
import Evergreen.V1.IdDict
import Evergreen.V1.Postmark
import Evergreen.V1.SurveyName
import List.Nonempty
import String.Nonempty
import Url


type HasSubmitted
    = HasPressedSubmit
    | HasNotPressedSubmit


type SubmitState a
    = NotSubmitted HasSubmitted
    | Submitting a


type alias AnsweringSurvey2 =
    { surveyId : Evergreen.V1.Id.Id Evergreen.V1.Id.SurveyId
    , userToken : Evergreen.V1.Id.Id Evergreen.V1.Id.UserToken
    , emailAddress : Evergreen.V1.EmailAddress.EmailAddress
    , title : Evergreen.V1.SurveyName.SurveyName
    , answers :
        List.Nonempty.Nonempty
            { question : String
            , answer : String
            }
    , submitState : SubmitState ()
    }


type alias CreatingSurvey2 =
    { surveyName : String
    , questions : List.Nonempty.Nonempty String
    , emailTo : String
    , submitState :
        SubmitState
            { surveyName : Evergreen.V1.SurveyName.SurveyName
            , questions : List.Nonempty.Nonempty String
            , emailTo : List.Nonempty.Nonempty Evergreen.V1.EmailAddress.EmailAddress
            }
    }


type alias SurveyQuestion =
    { question : String
    , answers : AssocList.Dict Evergreen.V1.EmailAddress.EmailAddress String.Nonempty.NonemptyString
    }


type EmailStatus
    = SendingEmail
    | EmailError Effect.Http.Error
    | EmailSuccess Evergreen.V1.Postmark.PostmarkSendResponse


type alias FrontendSurvey =
    { title : Evergreen.V1.SurveyName.SurveyName
    , questions : List.Nonempty.Nonempty SurveyQuestion
    , emailedTo :
        List.Nonempty.Nonempty
            ( Evergreen.V1.Id.Id Evergreen.V1.Id.UserToken
            , { email : Evergreen.V1.EmailAddress.EmailAddress
              , emailStatus : EmailStatus
              }
            )
    }


type LoadSurveyError
    = InvalidSurveyLink
    | SurveyAlreadySubmitted


type FrontendState
    = LoadingSurvey (Evergreen.V1.Id.Id Evergreen.V1.Id.SurveyId) (Evergreen.V1.Id.Id Evergreen.V1.Id.UserToken)
    | AnsweringSurvey AnsweringSurvey2
    | SubmittedSurvey
    | CreatingSurvey CreatingSurvey2
    | SurveyOverviewAdmin (Evergreen.V1.Id.Id Evergreen.V1.Id.SurveyId) FrontendSurvey
    | LoadingSurveyFailed LoadSurveyError


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , state : FrontendState
    }


type alias BackendSurvey =
    { title : Evergreen.V1.SurveyName.SurveyName
    , questions : List.Nonempty.Nonempty SurveyQuestion
    , emailedTo :
        List.Nonempty.Nonempty
            ( Evergreen.V1.Id.Id Evergreen.V1.Id.UserToken
            , { email : Evergreen.V1.EmailAddress.EmailAddress
              , emailStatus : EmailStatus
              }
            )
    , owner : Evergreen.V1.Id.Id Evergreen.V1.Id.UserToken
    }


type alias BackendModel =
    { surveys : Evergreen.V1.IdDict.IdDict Evergreen.V1.Id.SurveyId BackendSurvey
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
        (Evergreen.V1.Id.Id Evergreen.V1.Id.SurveyId)
        (Evergreen.V1.Id.Id Evergreen.V1.Id.UserToken)
        (List.Nonempty.Nonempty
            { answer : String
            }
        )
    | CreateSurveyRequest
        Evergreen.V1.SurveyName.SurveyName
        (List.Nonempty.Nonempty
            { question : String
            }
        )
        (List.Nonempty.Nonempty Evergreen.V1.EmailAddress.EmailAddress)
    | LoadSurveyRequest (Evergreen.V1.Id.Id Evergreen.V1.Id.SurveyId) (Evergreen.V1.Id.Id Evergreen.V1.Id.UserToken)


type BackendMsg
    = SurveyEmailSent (Evergreen.V1.Id.Id Evergreen.V1.Id.SurveyId) Evergreen.V1.EmailAddress.EmailAddress (Result Effect.Http.Error Evergreen.V1.Postmark.PostmarkSendResponse)


type ToFrontend
    = SubmitSurveyResponse
    | CreateSurveyResponse
        (Evergreen.V1.Id.Id Evergreen.V1.Id.SurveyId)
        (Evergreen.V1.Id.Id Evergreen.V1.Id.UserToken)
        (List.Nonempty.Nonempty
            ( Evergreen.V1.Id.Id Evergreen.V1.Id.UserToken
            , { email : Evergreen.V1.EmailAddress.EmailAddress
              , emailStatus : EmailStatus
              }
            )
        )
    | LoadSurveyResponse
        (Result
            LoadSurveyError
            { surveyId : Evergreen.V1.Id.Id Evergreen.V1.Id.SurveyId
            , userToken : Evergreen.V1.Id.Id Evergreen.V1.Id.UserToken
            , emailAddress : Evergreen.V1.EmailAddress.EmailAddress
            , surveyName : Evergreen.V1.SurveyName.SurveyName
            , questions :
                List.Nonempty.Nonempty
                    { question : String
                    }
            }
        )
    | LoadSurveyAdminResponse (Evergreen.V1.Id.Id Evergreen.V1.Id.SurveyId) (Result () FrontendSurvey)
