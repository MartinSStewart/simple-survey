module Survey exposing (BackendSurvey, EmailStatus(..), FrontendSurvey, SurveyQuestion, hasSubmitted)

import AssocList as Dict exposing (Dict)
import Effect.Http as Http
import EmailAddress exposing (EmailAddress)
import Id exposing (Id, UserToken)
import List.Nonempty exposing (Nonempty)
import Postmark exposing (PostmarkSendResponse)
import String.Nonempty exposing (NonemptyString)
import SurveyName exposing (SurveyName)
import Time


type alias BackendSurvey =
    { title : SurveyName
    , questions : Nonempty SurveyQuestion
    , emailedTo : Nonempty ( Id UserToken, { email : EmailAddress, emailStatus : EmailStatus } )
    , owner : Id UserToken
    , creationTime : Time.Posix
    }


type alias FrontendSurvey =
    { title : SurveyName
    , questions : Nonempty SurveyQuestion
    , emailedTo : Nonempty ( Id UserToken, { email : EmailAddress, emailStatus : EmailStatus } )
    }


type alias SurveyQuestion =
    { question : String, answers : Dict EmailAddress NonemptyString }


type EmailStatus
    = SendingEmail
    | EmailError Http.Error
    | EmailSuccess PostmarkSendResponse


hasSubmitted : EmailAddress -> { a | questions : Nonempty SurveyQuestion } -> Bool
hasSubmitted emailAddress survey =
    List.Nonempty.any (\{ answers } -> Dict.keys answers |> List.any ((==) emailAddress)) survey.questions
