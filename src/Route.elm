module Route exposing (..)

import Id exposing (Id, SurveyId, UserToken)
import Url.Builder
import Url.Parser exposing ((</>))


type Route
    = CreateSurvey
    | ViewSurvey (Id SurveyId) (Id UserToken)


decode : Url.Parser.Parser (Route -> c) c
decode =
    Url.Parser.oneOf
        [ Url.Parser.top |> Url.Parser.map CreateSurvey
        , Url.Parser.s secretLinkPath </> idSegment </> idSegment |> Url.Parser.map ViewSurvey
        ]


encode : Route -> String
encode route =
    Url.Builder.absolute
        (case route of
            CreateSurvey ->
                []

            ViewSurvey surveyId userToken ->
                [ secretLinkPath, Id.toString surveyId, Id.toString userToken ]
        )
        []


idSegment : Url.Parser.Parser (Id a -> c) c
idSegment =
    Url.Parser.map Id.fromString Url.Parser.string


secretLinkPath =
    "secret-link"
