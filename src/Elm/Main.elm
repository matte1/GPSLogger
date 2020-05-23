module Main exposing (main)

import Fitness.Autogen.Fitness as Fitness
import Fitness.Autogen.Running as Running
import Route exposing (..)

import Browser
import Browser.Navigation as Nav
import Html
import Url
import Html.Styled exposing (a, b, li, ul, text, toUnstyled)
import Html.Styled as Styled
import Html.Styled.Attributes exposing (class, css, href, src)
import Html.Styled.Events exposing (onClick)

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , page : Route
  }

type Msg
  = ClickedLink Browser.UrlRequest
  | ChangedUrl Url.Url

main : Program () Model Msg
main =
  let init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
      init flags url key = ( Model key url Home, Cmd.none )
  in Browser.application
     { init = init
     , view = view
     , update = update
     , subscriptions = \_ -> Sub.none
     , onUrlChange = ChangedUrl
     , onUrlRequest = ClickedLink
     }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let changeRouteTo : Maybe Route -> ( Model, Cmd Msg )
      changeRouteTo maybeRoute =
          case maybeRoute of
              Nothing -> ( {model | page = Home }, Cmd.none)
              Just Route.Home -> ( { model | page = Home }, Cmd.none)
              Just Route.Running -> ( { model | page = Running }, Cmd.none)
  in case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing -> ( model, Cmd.none )
                        Just _ -> changeRouteTo (Route.fromUrl url)
                Browser.External href ->
                    ( model
                    , Nav.load href
                    )
        ChangedUrl url ->
          changeRouteTo (Route.fromUrl url)

view : Model -> Browser.Document Msg
view model =
  let viewLink : Route -> Styled.Html msg
      viewLink route = li [] [ a [ Route.href route ] [ text (Route.routeToString route) ] ]
  in case model.page of
      Home ->
        { title = "Life Of Matt"
        , body =
            List.map toUnstyled
            [ ul []
                [ viewLink Home
                , viewLink Running
                ]
            , Fitness.view
            ]
        }
      Running ->
        { title = "Running"
        , body = [Running.view]
        }
