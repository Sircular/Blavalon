module Page.User exposing (Model, Msg, init, update, view)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Spinner as Spinner
import Html exposing (Html, div, h1, p, small, text)
import Html.Attributes exposing (class, classList, for, href)
import Http
import Session exposing (Session)
import Utils exposing (cond, textInput)
import WebData exposing (WebData)


helpText =
    "Please use your real name. Your group needs to be able to identify "
        ++ "you without having to ask who you are."


rulesText =
    " Your name must be greater than 2 characters and less than 40, and "
        ++ "cannot contain any special characters other than space. "


type alias Model =
    { session : Session
    , username : String
    , response : WebData ()
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , username = ""
      , response = WebData.NotLoaded
      }
    , Cmd.none
    )


type Msg
    = UpdateUsername String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        Submit ->
            case validateUsername model.username of
                Nothing ->
                    ( { model | response = WebData.Loading }, Cmd.none )

                Just error ->
                    ( { model | response = WebData.Error (Http.BadBody error) }
                    , Cmd.none
                    )


validateUsername : String -> Maybe String
validateUsername username =
    let
        trimmed =
            String.trim username
    in
    if String.length trimmed <= 2 then
        Just "Username too short."

    else if String.length trimmed >= 40 then
        Just "Username too long."

    else
        Nothing


view : Model -> Html Msg
view model =
    let
        errorText =
            case model.response of
                WebData.Error (Http.BadBody body) ->
                    Just body

                _ ->
                    Nothing

        isLoading =
            case model.response of
                WebData.Loading ->
                    True

                _ ->
                    False
    in
    div []
        [ div [ class "jumbotron" ]
            [ h1 [ class "display-4" ] [ text "Spamalot" ]
            , p [ class "lead" ] [ text "Faster Avalon Roles" ]
            ]
        , Grid.simpleRow
            [ Grid.col []
                [ h1 [] [ text "Join a Game" ]
                , p [] [ text helpText ]
                , Form.form []
                    [ textInput
                        { id = "username"
                        , value = model.username
                        , onInput = UpdateUsername
                        , name = "Your Name"
                        , helpText = rulesText
                        , disabled = isLoading
                        , dangerText = errorText
                        }
                    , Button.button
                        [ Button.primary
                        , Button.onClick Submit
                        , Button.disabled isLoading
                        ]
                        [ text "Join Game" ]
                    ]
                , Spinner.spinner [ Spinner.large ] [] |> cond isLoading

                -- , cond isLoading <|
                --     Spinner.spinner [ Spinner.large ] []
                ]
            ]
        ]
