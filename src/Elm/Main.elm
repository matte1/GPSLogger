module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Styled exposing (toUnstyled)
import Url
import Running.Page as Running
import Route exposing (..)

-- MAIN
main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    , onUrlChange = ChangedUrl
    , onUrlRequest = ClickedLink
    }


-- MODEL
type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , page : Route
  }

-- UPDATE
type Msg
  = ClickedLink Browser.UrlRequest
  | ChangedUrl Url.Url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key = ( Model key url Home, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            Debug.log("ClickedLink: " ++ (Url.toString url))
                            changeRouteTo (Route.fromUrl url) model

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ChangedUrl url ->
          Debug.log("ChangedUrl: " ++ (Url.toString url))
          changeRouteTo (Route.fromUrl url) model

changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            Debug.log("changeRouteTo: Nothing...")
            ( {model | page = Home }, Cmd.none)

        Just Route.Home ->
            Debug.log("changeRouteTo: Home")
            ( { model | page = Home }, Cmd.none)

        Just Route.Running ->
            Debug.log("changeRouteTo: Running")
            ( { model | page = Running }, Cmd.none)

-- VIEW
view : Model -> Browser.Document Msg
view model =
  case model.page of
    Home ->
      Debug.log("View: Home")

      { title = "Home!"
      , body =
          [ text "The current URL is: "
          , b [] [ text (Url.toString model.url) ]
          , ul []
              [ viewLink Home
              , viewLink Running
              ]
          ]
      }
    Running ->
      Debug.log("View: Running")
      { title = "Running"
      , body = [Running.view model]
      }


viewLink : Route -> Html msg
viewLink route =
    li [] [ a [ Route.href route ] [ text (Route.routeToString route) ] ]
