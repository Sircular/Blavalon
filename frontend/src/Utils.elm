module Utils exposing (..)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Html exposing (Html, div, small, text)
import Html.Attributes exposing (for, style)
import Json.Encode as E
import Maybe exposing (withDefault)


curry : (( a, b ) -> c) -> a -> b -> c
curry f a b =
    f ( a, b )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe f a =
    withDefault E.null <| Maybe.map f a


cond : Bool -> Html a -> Html a
cond show node =
    if show then
        node

    else
        text ""


type alias TextInputConfig msg =
    { id : String
    , value : String
    , onInput : String -> msg
    , name : String
    , helpText : String
    , disabled : Bool
    , dangerText : Maybe String
    }


textInput : TextInputConfig a -> Html a
textInput config =
    Form.group []
        [ Form.label [ for config.id ] [ text config.name ]
        , Input.text <|
            List.filterMap identity
                [ Just (Input.id config.id)
                , Just (Input.value config.value)
                , Just (Input.onInput config.onInput)
                , Just (Input.disabled config.disabled)
                , Maybe.map (\_ -> Input.danger) config.dangerText
                ]
        , case config.dangerText of
            Just danger ->
                Form.invalidFeedback [] [ text danger ]

            Nothing ->
                Form.help [] [ text config.helpText ]
        ]
