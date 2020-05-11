module Running.Page exposing (view)

-- import Css exposing (..)
-- import Html.Styled exposing (..)
-- import Browser
-- import Browser.Navigation as Nav
-- import Url

import Tuple exposing (first, second)
import List exposing (map)
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

-- styledH1 :  List (Attribute msg) -> List (Html msg) -> Html msg
-- styledH1 =
--     styled h1
--     [ Css.color (Css.rgb 125 125 125) ]
-- view : model -> Html msg
-- view model = styledH1 [] [ text "Running" ]

-- type alias Model =
  -- { selector : Int
  -- }

view : model -> Html.Html msg
view _ =
  Html.div
    [ class "container" ]
    [ plot .miles
    , heartRateZones year1
    , heartRateZones year2
    , heartRateZones year3
    ]

plot : (WeeklyRunning -> Float) -> Html.Html msg
plot selector =
  LineChart.viewCustom
    { y = Axis.default 450 "Data" selector
    , x = Axis.default 700 "Week" .week
    , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
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
    [ LineChart.line Colors.pink Dots.none "2018" year1
    , LineChart.line Colors.blue Dots.none "2019" year2
    , LineChart.line Colors.cyan Dots.none "2020" year3
    ]

heartRateZones : List WeeklyRunning -> Html.Html msg
heartRateZones year =
  LineChart.viewCustom
    { y = Axis.default 450 "Heart Rate Zones" first
    , x = Axis.default 700 "Week" second
    , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.stacked 0.5
    , line = Line.default
    , dots = Dots.default
    }
    [ LineChart.line Colors.pink Dots.none "zone1" (map (\wr -> (wr.zone1, wr.week)) year)
    , LineChart.line Colors.blue Dots.none "zone2" (map (\wr -> (wr.zone2, wr.week)) year)
    , LineChart.line Colors.cyan Dots.none "zone3" (map (\wr -> (wr.zone3, wr.week)) year)
    , LineChart.line Colors.red Dots.none "zone4" (map (\wr -> (wr.zone4, wr.week)) year)
    , LineChart.line Colors.green Dots.none "zone5" (map (\wr -> (wr.zone5, wr.week)) year)
    ]

-- DATA
type alias WeeklyRunning =
  { week : Float
  , miles : Float
  , elevation : Float
  , totalTime : Float
  , zone1 : Float
  , zone2 : Float
  , zone3 : Float
  , zone4 : Float
  , zone5 : Float
  }

year1 : List WeeklyRunning
year1 =
    [ WeeklyRunning 4 45.59 0.0 6.08 0.48 1.32 2.76 1.31 0.21
    , WeeklyRunning 5 20.60 0.0 2.83 0.28 1.64 0.70 0.21 0.00
    , WeeklyRunning 6 44.84 0.0 6.08 1.03 2.34 1.87 0.83 0.00
    , WeeklyRunning 7 9.77 0.0 1.24 0.11 0.33 0.73 6.78e-02 0.00
    , WeeklyRunning 8 39.08 0.0 5.02 0.14 1.74 2.54 0.59 1.39e-03
    , WeeklyRunning 9 21.63 0.0 2.91 0.23 1.17 1.50 7.50e-03 0.00
    , WeeklyRunning 10 21.28 0.0 2.83 0.12 0.68 1.77 0.27 0.00
    , WeeklyRunning 11 33.01 0.0 4.32 0.20 1.74 2.19 0.19 0.00
    , WeeklyRunning 12 16.13 0.0 2.06 5.72e-02 0.68 0.86 0.46 0.00
    , WeeklyRunning 13 31.58 0.0 4.96 0.66 1.36 1.79 0.91 0.25
    , WeeklyRunning 14 17.88 0.0 2.56 0.79 0.50 1.08 0.17 2.75e-02
    , WeeklyRunning 15 16.30 0.0 2.99 0.68 0.76 1.35 0.20 3.33e-03
    , WeeklyRunning 16 33.15 0.0 5.00 0.63 1.74 2.21 0.39 2.92e-02
    , WeeklyRunning 17 25.77 0.0 3.43 0.16 2.18 0.92 0.18 0.00
    , WeeklyRunning 18 36.65 0.0 5.37 0.51 2.63 1.81 0.42 0.00
    , WeeklyRunning 19 19.44 0.0 2.77 0.40 1.27 0.76 0.25 8.83e-02
    , WeeklyRunning 21 27.40 0.0 4.12 1.22 0.98 1.35 0.57 8.33e-04
    , WeeklyRunning 22 18.98 0.0 2.63 0.29 1.01 1.08 0.25 0.00
    , WeeklyRunning 23 19.71 0.0 2.55 0.23 1.25 0.40 0.33 0.34
    , WeeklyRunning 24 18.34 0.0 2.48 0.58 0.83 0.53 0.28 0.26
    , WeeklyRunning 25 24.91 0.0 3.27 0.17 0.47 1.00 1.36 0.27
    , WeeklyRunning 26 13.93 0.0 2.96 1.47 0.80 0.69 5.56e-03 0.00
    , WeeklyRunning 27 17.76 0.0 2.77 0.95 1.18 0.35 0.28 0.00
    , WeeklyRunning 28 25.29 0.0 3.59 0.22 1.57 1.50 0.31 0.00
    , WeeklyRunning 29 19.50 0.0 4.88 2.79 0.72 0.70 0.57 0.10
    , WeeklyRunning 30 4.35 0.0 1.31 0.91 0.37 2.81e-02 0.00 0.00
    , WeeklyRunning 31 25.51 0.0 3.43 0.74 0.79 1.14 0.63 0.13
    , WeeklyRunning 32 4.06 0.0 0.51 3.75e-02 0.12 0.36 0.00 0.00
    , WeeklyRunning 33 11.68 0.0 1.62 0.12 0.22 0.44 0.74 0.10
    , WeeklyRunning 34 9.53 0.0 1.25 4.53e-02 0.20 0.26 0.74 0.00
    , WeeklyRunning 35 9.76 0.0 2.36 1.31 0.11 0.72 0.22 0.00
    , WeeklyRunning 37 36.26 0.0 10.03 2.03 2.34 3.28 2.35 3.33e-02
    , WeeklyRunning 38 20.65 0.0 4.22 1.94 1.00 1.19 0.10 0.00
    , WeeklyRunning 39 33.23 0.0 5.14 1.56 1.27 1.46 0.85 0.00
    , WeeklyRunning 40 14.27 0.0 2.58 1.18 1.15 0.24 0.00 0.00
    , WeeklyRunning 41 10.71 0.0 1.82 0.80 0.61 0.31 9.00e-02 6.11e-03
    , WeeklyRunning 42 13.19 0.0 2.12 0.21 0.53 0.80 0.57 0.00
    , WeeklyRunning 43 21.30 0.0 3.18 1.18 1.21 0.74 5.31e-02 0.00
    , WeeklyRunning 44 19.25 0.0 3.27 0.71 1.01 0.88 0.65 1.89e-02
    , WeeklyRunning 45 11.85 0.0 1.65 0.29 0.33 0.72 0.30 0.00
    , WeeklyRunning 46 5.66 0.0 0.88 8.97e-02 0.30 0.49 0.00 0.00
    , WeeklyRunning 48 17.07 0.0 2.68 0.45 1.20 0.97 6.69e-02 0.00
    , WeeklyRunning 49 12.58 0.0 1.56 0.12 0.29 1.00 0.15 0.00
    , WeeklyRunning 50 9.94 0.0 1.42 5.28e-02 0.22 0.83 0.33 0.00
    , WeeklyRunning 51 19.91 0.0 2.48 0.13 0.71 1.36 0.28 0.00
    , WeeklyRunning 52 5.20 0.0 0.79 5.06e-02 0.18 0.38 0.18 0.00
    ]

year2 : List WeeklyRunning
year2 =
  [ WeeklyRunning 1 4.03 0.0 0.52 9.44e-03 0.28 0.23 0.00 0.00
  , WeeklyRunning 4 4.30 0.0 0.59 0.13 0.18 0.29 0.00 0.00
  , WeeklyRunning 5 26.31 0.0 3.70 0.32 1.33 1.72 0.34 0.00
  , WeeklyRunning 6 10.80 0.0 1.36 9.89e-02 0.42 0.71 0.14 0.00
  , WeeklyRunning 7 6.83 0.0 0.90 0.15 0.46 0.25 4.25e-02 0.00
  , WeeklyRunning 8 9.67 0.0 1.14 0.19 0.45 0.34 0.16 0.00
  , WeeklyRunning 9 26.07 0.0 3.33 0.16 0.56 1.66 0.95 0.00
  , WeeklyRunning 10 13.05 0.0 1.66 0.21 0.71 0.49 0.26 0.00
  , WeeklyRunning 11 18.94 0.0 2.53 0.17 0.39 1.36 0.61 0.00
  , WeeklyRunning 12 13.58 0.0 1.76 0.32 0.19 0.98 0.26 0.00
  , WeeklyRunning 14 10.78 0.0 1.46 0.14 0.42 0.70 0.19 0.00
  , WeeklyRunning 15 19.77 0.0 2.55 0.25 1.07 0.90 0.32 0.00
  , WeeklyRunning 16 25.38 0.0 3.09 0.18 1.26 1.38 0.27 0.00
  , WeeklyRunning 17 24.04 0.0 3.18 0.12 0.71 2.13 0.22 0.00
  , WeeklyRunning 18 14.17 0.0 2.06 0.38 1.02 0.61 4.67e-02 0.00
  , WeeklyRunning 19 13.72 0.0 1.77 0.26 0.39 0.64 0.31 0.16
  , WeeklyRunning 20 20.70 0.0 2.99 0.43 1.04 1.22 0.31 1.11e-03
  , WeeklyRunning 21 6.79 0.0 0.94 3.22e-02 0.31 0.56 3.58e-02 0.00
  , WeeklyRunning 22 6.01 0.0 0.83 4.25e-02 0.52 0.27 0.00 0.00
  , WeeklyRunning 23 13.48 0.0 1.64 0.17 0.80 0.25 0.42 1.08e-02
  , WeeklyRunning 24 14.37 0.0 1.82 8.72e-02 0.30 1.38 5.75e-02 0.00
  , WeeklyRunning 25 22.18 0.0 3.59 0.42 1.22 1.29 0.58 8.50e-02
  , WeeklyRunning 26 33.25 0.0 6.12 0.67 2.83 1.90 0.70 1.31e-02
  , WeeklyRunning 27 32.58 0.0 4.37 0.28 1.28 1.49 1.28 4.42e-02
  , WeeklyRunning 28 16.16 0.0 2.06 0.10 0.62 1.20 0.14 0.00
  , WeeklyRunning 29 13.26 0.0 1.60 5.92e-02 0.84 0.19 0.51 5.56e-04
  , WeeklyRunning 30 22.29 0.0 4.31 2.25e-02 0.46 2.08 1.73 1.92e-02
  , WeeklyRunning 31 36.46 0.0 4.93 0.47 2.19 1.70 0.56 0.00
  , WeeklyRunning 32 18.49 0.0 2.32 0.44 0.47 1.37 4.44e-02 0.00
  , WeeklyRunning 33 27.82 0.0 4.47 1.31 0.69 1.37 1.08 3.44e-02
  , WeeklyRunning 34 24.44 0.0 3.02 4.42e-02 0.19 1.38 1.41 0.00
  , WeeklyRunning 35 19.00 0.0 2.58 0.27 1.09 1.11 0.11 0.00
  , WeeklyRunning 36 22.58 0.0 3.02 0.19 0.96 1.22 0.65 0.00
  , WeeklyRunning 37 35.44 0.0 5.05 0.49 2.97 0.72 0.82 5.36e-02
  , WeeklyRunning 38 30.27 0.0 4.29 1.06 2.29 0.52 0.43 0.00
  , WeeklyRunning 39 48.15 0.0 6.25 0.72 2.88 1.99 0.67 0.00
  , WeeklyRunning 40 23.55 0.0 3.10 0.58 1.88 0.63 0.00 0.00
  , WeeklyRunning 41 18.16 0.0 2.80 0.79 1.58 0.43 0.00 0.00
  , WeeklyRunning 42 6.52 0.0 0.76 5.19e-02 0.14 0.57 0.00 0.00
  , WeeklyRunning 43 2.68 0.0 0.34 5.64e-02 0.11 0.18 0.00 0.00
  , WeeklyRunning 44 13.11 0.0 1.87 8.00e-02 0.67 1.02 0.10 0.00
  , WeeklyRunning 45 33.48 0.0 4.60 0.44 1.34 1.86 0.90 6.81e-02
  , WeeklyRunning 46 6.21 0.0 0.65 5.08e-02 0.11 0.15 0.34 0.00
  , WeeklyRunning 48 21.02 0.0 2.63 0.12 0.42 1.54 0.55 0.00
  , WeeklyRunning 49 38.78 0.0 5.40 0.50 2.42 1.20 1.28 0.00
  , WeeklyRunning 50 21.74 0.0 2.49 0.17 0.30 1.57 0.45 0.00
  , WeeklyRunning 51 7.78 0.0 1.04 5.58e-02 0.71 0.28 0.00 0.00
  , WeeklyRunning 52 7.72 0.0 1.10 0.13 0.35 0.56 6.44e-02 0.00
  ]

year3 : List WeeklyRunning
year3 =
  [ WeeklyRunning 1 10.37 0.0 1.84 0.31 0.28 0.55 0.69 3.33e-03
  , WeeklyRunning 2 27.49 0.0 3.71 0.47 0.93 1.93 0.37 0.00
  , WeeklyRunning 3 18.58 0.0 2.31 0.25 1.26 0.80 0.00 0.00
  , WeeklyRunning 6 11.83 0.0 1.60 0.13 1.30 0.18 0.00 0.00
  , WeeklyRunning 7 36.52 0.0 4.98 0.26 2.90 1.51 0.31 0.00
  , WeeklyRunning 8 27.10 0.0 4.70 3.02 1.32 0.36 0.00 0.00
  , WeeklyRunning 9 43.49 0.0 5.71 0.55 3.84 1.32 0.00 0.00
  , WeeklyRunning 10 32.40 0.0 4.28 0.47 3.52 0.23 2.83e-02 2.72e-02
  , WeeklyRunning 11 52.33 0.0 6.96 0.93 5.62 0.29 0.11 0.00
  , WeeklyRunning 12 16.60 0.0 2.07 0.30 1.15 0.41 0.21 0.00
  , WeeklyRunning 13 41.07 0.0 6.95 1.67 3.96 1.27 4.86e-02 0.00
  , WeeklyRunning 14 42.07 0.0 7.53 2.62 4.38 0.53 2.78e-04 0.00
  , WeeklyRunning 15 43.45 0.0 6.29 1.86 3.25 0.82 0.36 0.00
  , WeeklyRunning 16 41.58 0.0 6.27 1.90 3.63 0.74 0.00 0.00
  , WeeklyRunning 17 45.70 0.0 6.45 2.27 3.05 0.97 0.15 0.00
  , WeeklyRunning 18 44.15 0.0 6.23 0.47 3.66 1.83 0.26 2.22e-03
  ]
