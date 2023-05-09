module Route exposing (..)

import Id exposing (Id, SurveyId, UserToken)
import Url.Parser exposing ((</>))


type Route
    = CreateSurvey
    | ViewSurvey (Id SurveyId) (Id UserToken)


decode : Url.Parser.Parser (Route -> c) c
decode =
    Url.Parser.oneOf
        [ Url.Parser.top |> Url.Parser.map CreateSurvey
        , Url.Parser.s "secret-link" </> idSegment </> idSegment |> Url.Parser.map ViewSurvey
        ]


idSegment : Url.Parser.Parser (Id a -> c) c
idSegment =
    Url.Parser.map Id.fromString Url.Parser.string
