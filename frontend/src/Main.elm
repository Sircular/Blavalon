port module Main exposing (..)

import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, div, footer, text)
import Html.Attributes exposing (class, classList, href, id)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Maybe exposing (withDefault)
import Page.User as User
import Session exposing (Session, User)
import Url exposing (Url)
import Utils exposing (uncurry)


main =
    Browser.application
        { init = init
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , update = updateWithStore
        , view = \model -> { title = "Spamalon", body = view model }
        }


port storeSession : E.Value -> Cmd msg


init : E.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init user url key =
    let
        parsedUser =
            case D.decodeValue Session.decodeUser user of
                Ok val ->
                    val

                Err _ ->
                    Session.Guest

        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        ( pageModel, pageCmd ) =
            User.init
                { key = key
                , url = url
                , user = parsedUser
                }
    in
    ( { navbarState = navbarState
      , pageModel = UserModel pageModel
      }
    , Cmd.batch
        [ navbarCmd
        , Cmd.map UserMsg pageCmd
        ]
    )


type alias Model =
    { navbarState : Navbar.State
    , pageModel : PageModel
    }


type PageModel
    = Loading Session
    | UserModel User.Model


getSession : Model -> Session
getSession model =
    case model.pageModel of
        Loading session ->
            session

        UserModel userModel ->
            userModel.session


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | NavbarMsg Navbar.State
    | UserMsg User.Msg


updateWithStore : Msg -> Model -> ( Model, Cmd Msg )
updateWithStore msg model =
    let
        ( newModel, newCmd ) =
            update msg model

        newSession =
            getSession newModel
    in
    ( newModel
    , Cmd.batch
        [ newCmd
        , storeSession (Session.encodeUser newSession.user)
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.pageModel ) of
        ( UrlChanged request, _ ) ->
            ( model, Cmd.none )

        ( LinkClicked request, _ ) ->
            case request of
                Browser.Internal url ->
                    ( model, Cmd.none )

                Browser.External href ->
                    ( model, Nav.load href )

        ( NavbarMsg state, _ ) ->
            ( { model | navbarState = state }, Cmd.none )

        ( UserMsg userMsg, UserModel userModel ) ->
            updatePage UserMsg UserModel model <| User.update userMsg userModel

        ( _, _ ) ->
            ( model, Cmd.none )


updatePage :
    (msg -> Msg)
    -> (model -> PageModel)
    -> Model
    -> ( model, Cmd msg )
    -> ( Model, Cmd Msg )
updatePage mapMsg mapModel model ( pageModel, cmd ) =
    ( { model | pageModel = mapModel pageModel }, Cmd.map mapMsg cmd )



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ viewHeader model
    , Html.main_
        [ class "container"
        , id "main-container"
        ]
        [ viewPage model.pageModel ]
    , viewFooter
    ]


viewPage : PageModel -> Html Msg
viewPage pageModel =
    case pageModel of
        Loading _ ->
            text "Loading"

        UserModel model ->
            Html.map UserMsg (User.view model)


viewHeader : Model -> Html Msg
viewHeader navbarModel =
    Navbar.config NavbarMsg
        |> Navbar.dark
        |> Navbar.withAnimation
        |> Navbar.attrs [ Spacing.mb3 ]
        |> Navbar.brand [ href "#" ] [ text "Spamalot" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "/" ] [ text "Home" ]
            , Navbar.itemLink [ href "https://github.com/DylanDmitri/spamalot" ] [ text "Github" ]
            ]
        |> Navbar.view navbarModel.navbarState


viewFooter : Html Msg
viewFooter =
    footer [ class "footer" ]
        [ div [ class "container" ]
            [ a
                [ href "https://github.com/DylanDmitri/spamalot/issues"
                , class "text-muted"
                ]
                [ text "Report Issues" ]
            ]
        ]
