module Plotting.BarCharts exposing (stackedBarChart)

import Html
import Axis
import Color exposing (Color)
import List.Extra as List
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Scale.Color
import Shape exposing (StackConfig, StackResult)
import TypedSvg exposing (g, rect, svg, text_, tspan)
import TypedSvg.Attributes exposing (class, fill, fontWeight, stroke, style, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Paint(..), Transform(..))


stackedBarChart : Html.Html msg
stackedBarChart = viewStackedBarChart (Shape.stack config)

type alias Day = Int

series : List { label : String, accessor : Workout -> Float }
series =
    [ { label = "Running"
      , accessor = .running
      }
    , { label = "Climbing"
      , accessor = .climbing
      }
    , { label = "Stabilizers"
      , accessor = .stabilizers
      }
    ]


samples : List ( String, List Float )
samples = List.map (\{ label, accessor } -> ( label, List.map accessor workouts )) series

w : Float
w = 990

h : Float
h = 504

padding : { bottom : Float, left : Float, right : Float, top : Float }
padding =
    { top = 30
    , left = 60
    , right = 30
    , bottom = 60
    }


config : StackConfig String
config =
    { data = samples
    , offset = Shape.stackOffsetNone
    , order = identity
    }


reverseViridis : Float -> Color
reverseViridis progression =
    -- stylistic choice: the larger boxes look better in brighter colors, so invert the interpolator
    Scale.Color.viridisInterpolator (1 - progression)


colors : Int -> List Color
colors size =
    let
        colorScale =
            Scale.sequential reverseViridis ( 0, toFloat size - 1 )
                |> Scale.convert
    in
    List.range 0 (size - 1)
        |> List.map (colorScale << toFloat)

column : BandScale Day -> ( Day, List ( Float, Float ) ) -> Svg msg
column xScale ( year, values ) =
    let block color ( upperY, lowerY ) =
          rect
              [ x <| Scale.convert xScale year
              , y <| lowerY
              , width <| Scale.bandwidth xScale
              , height <| (abs <| upperY - lowerY)
              , fill (Paint color)
              ]
              []
    in g [ class [ "column" ] ] (List.map2 block (colors (List.length values)) values)

labelBarInStackedBars : BandScale Day -> ContinuousScale Float -> List Day -> String -> List (Float, Float) -> Svg msg
labelBarInStackedBars xScale yScale xs label lowHighs =
  let makeLabel : Day -> (Float, Float) -> Svg msg
      makeLabel day (yLow, yHigh) =
        let yMiddle = (yHigh-yLow) / 2 + yLow
        in text_
            [ transform [ Translate (Scale.convert xScale day) (Scale.convert yScale yMiddle) ]
            , x 135
            , y 45
            ]
            [ text <| if yHigh - yLow == 0 then "" else label ]
  in g [ style "font: bold 12px sans-serif; font-variant-numeric: tabular-nums;"
       , textAnchor AnchorEnd
       ] <| List.map2 makeLabel xs lowHighs

viewStackedBarChart : StackResult String -> Svg msg
viewStackedBarChart { values, labels, extent } =
    let days : List Day
        days = List.map .day workouts

        renderedLabels = List.map2 (labelBarInStackedBars xScale yScale days) labels values

        xScale : BandScale Day
        xScale =
          Scale.band
          { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
          ( 0, w - (padding.top + padding.bottom) )
          days

        yScale : ContinuousScale Float
        yScale =
          Scale.linear
          ( h - (padding.left + padding.right), 0 )
          extent |> (Scale.nice 4)

        scaledValues =
            List.map
            (List.map (\( y1, y2 ) -> ( Scale.convert yScale y1, Scale.convert yScale y2 )))
            (List.transpose values)

        xaxisTicks =
          g [ transform [ Translate (padding.left - 1) (h - padding.bottom) ] ]
            [ Axis.bottom [ Axis.tickCount 10 ] (Scale.toRenderable String.fromInt xScale) ]

        yaxisTicks =
          g [ transform [ Translate (padding.left - 1) padding.top ] ]
            [ Axis.left [] yScale ]

        barChart =
          let bars = List.map (column xScale) (List.map2 (\a b -> ( a, b )) days scaledValues)
          in g [ transform [ Translate padding.left padding.top ] ] bars

    in svg [ viewBox 0 0 w h ]
        (List.append
        [ xaxisTicks
        , yaxisTicks
        , barChart
        ] renderedLabels)

zip : List a -> List b -> List (a, b)
zip xs ys = List.map2 Tuple.pair xs ys

type alias Workout =
    { day : Int
    , running : Float
    , climbing : Float
    , stabilizers : Float
    }

workouts : List Workout
workouts =
    [ Workout 2 2.2 0 0.5
    , Workout 3 1.5 2.3 0.5
    , Workout 4 0 0 0
    , Workout 5 0 0 0
    , Workout 6 0 0 0
    , Workout 7 0 0 0
    , Workout 8 0 0 0
    ]
