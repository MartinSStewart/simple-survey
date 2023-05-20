module Id exposing
    ( Id(..)
    , SurveyId(..)
    , UserToken(..)
    , decoder
    , encode
    , fromString
    , getUniqueId
    , toString
    )

import Env
import Json.Decode
import Json.Encode
import Sha256
import Time


type Id a
    = Id String


toString : Id a -> String
toString (Id hash) =
    hash


fromString : String -> Id a
fromString =
    Id


decoder : Json.Decode.Decoder (Id a)
decoder =
    Json.Decode.map Id Json.Decode.string


encode : Id a -> Json.Encode.Value
encode (Id id) =
    Json.Encode.string id


getUniqueId : { a | secretCounter : Int } -> ( { a | secretCounter : Int }, Id b )
getUniqueId model =
    ( { model | secretCounter = model.secretCounter + 1 }
    , Env.secretKey
        ++ ":"
        ++ String.fromInt model.secretCounter
        |> Sha256.sha224
        |> String.left 20
        |> Id
    )


type UserToken
    = UserToken Never


type SurveyId
    = SurveyId Never
