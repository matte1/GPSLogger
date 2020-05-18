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

stackedBarChart : List Int -> List (String, List Float) -> Html.Html msg
stackedBarChart xs samples =
  let config : StackConfig String
      config =
          { data = samples
          , offset = Shape.stackOffsetNone
          , order = identity
          }

  in viewStackedBarChart xs (Shape.stack config)

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

column : BandScale Int -> ( Int, List ( Float, Float ) ) -> Svg msg
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

labelBarInStackedBars : BandScale Int -> ContinuousScale Float -> List Int -> String -> List (Float, Float) -> Svg msg
labelBarInStackedBars xScale yScale xs label lowHighs =
  let makeLabel : Int -> (Float, Float) -> Svg msg
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

viewStackedBarChart : List Int -> StackResult String -> Svg msg
viewStackedBarChart xs { values, labels, extent } =
    let renderedLabels = List.map2 (labelBarInStackedBars xScale yScale xs) labels values

        xScale : BandScale Int
        xScale =
          Scale.band
          { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
          ( 0, w - (padding.top + padding.bottom) )
          xs

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
          let bars = List.map (column xScale) (List.map2 (\a b -> ( a, b )) xs scaledValues)
          in g [ transform [ Translate padding.left padding.top ] ] bars

    in svg [ viewBox 0 0 w h ]
        (List.append
        [ xaxisTicks
        , yaxisTicks
        , barChart
        ] renderedLabels)

zip : List a -> List b -> List (a, b)
zip xs ys = List.map2 Tuple.pair xs ys
