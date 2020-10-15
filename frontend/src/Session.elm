module Session exposing (Session, User(..), decodeUser, encodeUser)

import Browser.Navigation as Nav
import Json.Decode as D
import Json.Encode as E
import Url
import Utils exposing (encodeMaybe)


type alias Session =
    { key : Nav.Key
    , url : Url.Url
    , user : User
    }


type User
    = Guest
    | LoggedIn LoggedInData


type alias LoggedInData =
    { name : String
    , token : String
    , room : Maybe String
    , owner : Bool
    }


decodeUser : D.Decoder User
decodeUser =
    D.oneOf
        [ D.map LoggedIn <|
            D.map4 LoggedInData
                (D.field "name" D.string)
                (D.field "token" D.string)
                (D.maybe (D.field "room" D.string))
                (D.field "owner" D.bool)
        , D.null Guest
        ]


encodeUser : User -> E.Value
encodeUser user =
    case user of
        Guest ->
            E.null

        LoggedIn data ->
            E.object
                [ ( "name", E.string data.name )
                , ( "token", E.string data.token )
                , ( "room", encodeMaybe E.string data.room )
                , ( "owner", E.bool data.owner )
                ]
