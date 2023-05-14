module Evergreen.V2.Survey exposing (..)

import AssocList
import Evergreen.V2.EmailAddress
import Evergreen.V2.Id
import Evergreen.V2.Postmark
import Evergreen.V2.SurveyName
import List.Nonempty
import String.Nonempty
import Time


type alias SurveyQuestion =
    { question : String
    , answers : AssocList.Dict Evergreen.V2.EmailAddress.EmailAddress String.Nonempty.NonemptyString
    }


type EmailStatus
    = SendingEmail
    | EmailError Evergreen.V2.Postmark.Error
    | EmailSuccess


type alias FrontendSurvey =
    { title : Evergreen.V2.SurveyName.SurveyName
    , questions : List.Nonempty.Nonempty SurveyQuestion
    , emailedTo :
        List.Nonempty.Nonempty
            ( Evergreen.V2.Id.Id Evergreen.V2.Id.UserToken
            , { email : Evergreen.V2.EmailAddress.EmailAddress
              , emailStatus : EmailStatus
              }
            )
    , owner : Evergreen.V2.Id.Id Evergreen.V2.Id.UserToken
    , creationTime : Time.Posix
    }


type alias BackendSurvey =
    { title : Evergreen.V2.SurveyName.SurveyName
    , questions : List.Nonempty.Nonempty SurveyQuestion
    , emailedTo :
        List.Nonempty.Nonempty
            ( Evergreen.V2.Id.Id Evergreen.V2.Id.UserToken
            , { email : Evergreen.V2.EmailAddress.EmailAddress
              , emailStatus : EmailStatus
              }
            )
    , owner : Evergreen.V2.Id.Id Evergreen.V2.Id.UserToken
    , creationTime : Time.Posix
    }
