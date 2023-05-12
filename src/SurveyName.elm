module SurveyName exposing (Error(..), SurveyName(..), fromString, maxLength, toNonemptyString, toString)

import String.Nonempty exposing (NonemptyString)


type SurveyName
    = SurveyName NonemptyString


type Error
    = SurveyNameTooShort
    | SurveyNameTooLong


minLength : number
minLength =
    1


maxLength : number
maxLength =
    50


fromString : String -> Result Error SurveyName
fromString text =
    let
        trimmed =
            String.trim text
    in
    case String.Nonempty.fromString trimmed of
        Just nonempty ->
            if String.length trimmed > maxLength then
                Err SurveyNameTooLong

            else
                Ok (SurveyName nonempty)

        Nothing ->
            Err SurveyNameTooShort


toString : SurveyName -> String
toString (SurveyName a) =
    String.Nonempty.toString a


toNonemptyString : SurveyName -> NonemptyString
toNonemptyString (SurveyName a) =
    a
