module Postmark exposing
    ( ApiKey
    , Error(..)
    , PostmarkEmailBody(..)
    , PostmarkSend
    , PostmarkSendResponse
    , PostmarkTemplateSendResponse
    , apiKey
    , endpoint
    , sendEmail
    , sendEmailTask
    , sendTemplateEmail
    )

import Effect.Command as Command exposing (Command)
import Effect.Http
import Effect.Task exposing (Task)
import Email.Html
import EmailAddress exposing (EmailAddress)
import Json.Decode as D
import Json.Encode as E
import List.Nonempty exposing (Nonempty)
import String.Nonempty exposing (NonemptyString)


endpoint : String
endpoint =
    "https://api.postmarkapp.com"


{-| A SendGrid API key. In order to send an email you must have one of these (see the readme for how to get one).
-}
type ApiKey
    = ApiKey String


{-| Create an API key from a raw string (see the readme for how to get one).
-}
apiKey : String -> ApiKey
apiKey apiKey_ =
    ApiKey apiKey_


type PostmarkEmailBody
    = BodyHtml Email.Html.Html
    | BodyText String
    | BodyBoth Email.Html.Html String



-- Plain send


type alias PostmarkSend =
    { from : { name : String, email : EmailAddress }
    , to : Nonempty { name : String, email : EmailAddress }
    , subject : NonemptyString
    , body : PostmarkEmailBody
    , messageStream : String
    }


{-| Possible error codes we might get back when trying to send an email.
Some are just normal HTTP errors and others are specific to the Postmark API.
-}
type Error
    = UnknownError { statusCode : Int, body : String }
    | PostmarkError { errorCode : Int, message : String }
    | NetworkError
    | Timeout
    | BadUrl String


sendEmailTask : ApiKey -> PostmarkSend -> Effect.Task.Task restriction Error ()
sendEmailTask (ApiKey token) d =
    let
        httpBody =
            Effect.Http.jsonBody <|
                E.object <|
                    [ ( "From", E.string <| emailToString d.from )
                    , ( "To", E.string <| emailsToString d.to )
                    , ( "Subject", E.string <| String.Nonempty.toString d.subject )
                    , ( "MessageStream", E.string d.messageStream )
                    ]
                        ++ bodyToJsonValues d.body
    in
    Effect.Http.task
        { method = "POST"
        , headers = [ Effect.Http.header "X-Postmark-Server-Token" token ]
        , url = endpoint ++ "/email"
        , body = httpBody
        , resolver = jsonResolver decodePostmarkSendResponse
        , timeout = Nothing
        }


sendEmail :
    (Result Error () -> msg)
    -> ApiKey
    -> PostmarkSend
    -> Command restriction toMsg msg
sendEmail msg token d =
    sendEmailTask token d |> Effect.Task.attempt msg


emailsToString : List.Nonempty.Nonempty { name : String, email : EmailAddress } -> String
emailsToString nonEmptyEmails =
    nonEmptyEmails
        |> List.Nonempty.toList
        |> List.map emailToString
        |> String.join ", "


emailToString : { name : String, email : EmailAddress } -> String
emailToString address =
    if address.name == "" then
        EmailAddress.toString address.email

    else
        address.name ++ " <" ++ EmailAddress.toString address.email ++ ">"


type alias PostmarkSendResponse =
    { errorCode : Int
    , message : String
    }


decodePostmarkSendResponse : D.Decoder PostmarkSendResponse
decodePostmarkSendResponse =
    D.map2 PostmarkSendResponse
        (D.field "ErrorCode" D.int)
        (D.field "Message" D.string)



-- Template send


type alias PostmarkTemplateSend =
    { token : String
    , templateAlias : String
    , templateModel : E.Value
    , from : String
    , to : String
    , messageStream : String
    }


sendTemplateEmail : PostmarkTemplateSend -> Effect.Task.Task restriction Error ()
sendTemplateEmail d =
    let
        httpBody =
            Effect.Http.jsonBody <|
                E.object <|
                    [ ( "From", E.string d.from )
                    , ( "To", E.string d.to )
                    , ( "MessageStream", E.string d.messageStream )
                    , ( "TemplateAlias", E.string d.templateAlias )
                    , ( "TemplateModel", d.templateModel )
                    ]
    in
    Effect.Http.task
        { method = "POST"
        , headers = [ Effect.Http.header "X-Postmark-Server-Token" d.token ]
        , url = endpoint ++ "/email/withTemplate"
        , body = httpBody
        , resolver = jsonResolver decodePostmarkTemplateSendResponse
        , timeout = Nothing
        }


type alias PostmarkTemplateSendResponse =
    { errorCode : Int
    , message : String
    }


decodePostmarkTemplateSendResponse =
    D.map2 PostmarkTemplateSendResponse
        (D.field "ErrorCode" D.int)
        (D.field "Message" D.string)



-- Helpers


bodyToJsonValues : PostmarkEmailBody -> List ( String, E.Value )
bodyToJsonValues body =
    case body of
        BodyHtml html ->
            [ ( "HtmlBody", E.string <| Tuple.first <| Email.Html.toString html ) ]

        BodyText text ->
            [ ( "TextBody", E.string text ) ]

        BodyBoth html text ->
            [ ( "HtmlBody", E.string <| Tuple.first <| Email.Html.toString html )
            , ( "TextBody", E.string text )
            ]


jsonResolver : D.Decoder { a | errorCode : Int, message : String } -> Effect.Http.Resolver restriction Error ()
jsonResolver decoder =
    Effect.Http.stringResolver
        (\response ->
            case response of
                Effect.Http.GoodStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Ok json ->
                            if json.errorCode == 0 then
                                Ok ()

                            else
                                PostmarkError { errorCode = json.errorCode, message = json.message } |> Err

                        Err _ ->
                            UnknownError { statusCode = metadata.statusCode, body = body } |> Err

                Effect.Http.BadUrl_ message ->
                    Err (BadUrl message)

                Effect.Http.Timeout_ ->
                    Err Timeout

                Effect.Http.NetworkError_ ->
                    Err NetworkError

                Effect.Http.BadStatus_ metadata body ->
                    Err (UnknownError { statusCode = metadata.statusCode, body = body })
        )
