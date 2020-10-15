module WebData exposing (WebData(..))

import Http


type WebData a
    = NotLoaded
    | Loading
    | Error Http.Error
    | Value a


fromHttp : Result Http.Error a -> WebData a
fromHttp result =
    case result of
        Ok value ->
            Value value

        Err err ->
            Error err


map : (a -> b) -> WebData a -> WebData b
map f data =
    case data of
        Value value ->
            Value (f value)

        Loading ->
            Loading

        NotLoaded ->
            NotLoaded

        Error err ->
            Error err
