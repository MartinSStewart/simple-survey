module Env exposing (..)

import Postmark


secretKey =
    "123"


postmarkApiKey_ =
    ""


postmarkApiKey =
    Postmark.apiKey postmarkApiKey_


domain =
    "http://localhost:8000"
