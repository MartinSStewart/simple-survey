module Evergreen.V5.Survey exposing (..)

import AssocList
import Evergreen.V5.EmailAddress
import Evergreen.V5.Id
import Evergreen.V5.Postmark
import Evergreen.V5.SurveyName
import List.Nonempty
import String.Nonempty
import Time


type alias SurveyQuestion =
    { question : String
    , answers : AssocList.Dict Evergreen.V5.EmailAddress.EmailAddress String.Nonempty.NonemptyString
    }


type EmailStatus
    = SendingEmail
    | EmailError Evergreen.V5.Postmark.Error
    | EmailSuccess


type alias FrontendSurvey =
    { title : Evergreen.V5.SurveyName.SurveyName
    , questions : List.Nonempty.Nonempty SurveyQuestion
    , emailedTo :
        List.Nonempty.Nonempty
            ( Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken
            , { email : Evergreen.V5.EmailAddress.EmailAddress
              , emailStatus : EmailStatus
              }
            )
    , owner : Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken
    , creationTime : Time.Posix
    }


type alias BackendSurvey =
    { title : Evergreen.V5.SurveyName.SurveyName
    , questions : List.Nonempty.Nonempty SurveyQuestion
    , emailedTo :
        List.Nonempty.Nonempty
            ( Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken
            , { email : Evergreen.V5.EmailAddress.EmailAddress
              , emailStatus : EmailStatus
              }
            )
    , owner : Evergreen.V5.Id.Id Evergreen.V5.Id.UserToken
    , creationTime : Time.Posix
    }
