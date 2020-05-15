module Plotting.Plotting exposing (simpleLinePlot)

import Html
import Html.Attributes exposing (class)
import LineChart
import LineChart.Dots as Dots
import LineChart as LineChart
import LineChart.Junk as Junk exposing (..)
import LineChart.Dots as Dots
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Interpolation as Interpolation
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis as Axis
import LineChart.Legends as Legends
import LineChart.Line as Line
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Legends as Legends
import LineChart.Area as Area

containerConfig : Container.Config msg
containerConfig =
  Container.custom
    { attributesHtml = []
    , attributesSvg = []
    , size = Container.relative
    , margin = Container.Margin 30 100 30 70
    , id = "line-chart-area"
    }

simpleLinePlot : String -> (String, (a -> Float)) -> (String, (a -> Float)) -> List a -> Html.Html msg
simpleLinePlot title (xTitle, x) (yTitle, y) data =
  LineChart.viewCustom
    { y = Axis.default 450 yTitle y
    , x = Axis.default 1270 xTitle x
    , container = containerConfig
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.normal 0.5
    , line = Line.default
    , dots = Dots.default
    }
    [ LineChart.line Colors.cyan Dots.none title data
    ]

-- TODO(matte): Get mutha fucking fancy!
-- view : Model -> Html.Html Msg
-- view model =
--     Html.div []
--       [ LineChart.viewCustom (chartConfig model)
--           [ LineChart.line Colors.pink Dots.diamond  "Nora" model.data.nora
--           , LineChart.line Colors.cyan Dots.circle   "Noah" model.data.noah
--           , LineChart.line Colors.blue Dots.triangle "Nina" model.data.nina
--           ]
--       ]
--
--   chartConfig : Model -> LineChart.Config Datum Msg
--   chartConfig model =
--     { y = Axis.default 450 "velocity" .velocity
--     , x = Axis.time 1270 "time" .time
--     , container = containerConfig
--     , interpolation = Interpolation.monotone
--     , intersection = Intersection.default
--     , legends = Legends.default
--     , events = Events.hoverMany Hint
--     , junk = Junk.hoverMany model.hinted formatX formatY
--     , grid = Grid.dots 1 Colors.gray
--     , area = Area.stacked 0.5
--     , line = Line.default
--     , dots = Dots.custom (Dots.empty 5 1)
--     }
--
--   containerConfig : Container.Config Msg
--   containerConfig =
--     Container.custom
--       { attributesHtml = []
--       , attributesSvg = []
--       , size = Container.relative
--       , margin = Container.Margin 30 100 30 70
--       , id = "line-chart-area"
--       }
--
--   formatX : Datum -> String
--   formatX datum =
--     Date.Format.format "%e. %b, %Y" (Date.fromTime datum.time)
--
--   formatY : Datum -> String
--   formatY datum = toString (round100 datum.velocity) ++ " m/s"
