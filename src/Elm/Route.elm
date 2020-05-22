module Route exposing (Route(..), fromUrl, href, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- ROUTING
type Route
    = Home
    | Running


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Home (s "home")
        , Parser.map Running (s "running")
        ]


-- PUBLIC HELPERS
href : Route -> Attribute msg
href targetRoute = Attr.href <| routeToString targetRoute

replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route = Nav.replaceUrl key (routeToString route)

fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


-- INTERNAL
routeToString : Route -> String
routeToString page = "#/" ++ String.join "/" (routeToPieces page)

routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            [ "home" ]

        Running ->
            [ "running" ]
