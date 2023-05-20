module Evergreen.V5.Postmark exposing (..)


type Error
    = UnknownError
        { statusCode : Int
        , body : String
        }
    | PostmarkError
        { errorCode : Int
        , message : String
        }
    | NetworkError
    | Timeout
    | BadUrl String
