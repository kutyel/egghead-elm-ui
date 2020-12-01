module Checkbox exposing (dashboardColor, grey, lightGrey, white)

import Element exposing (Color, Element, htmlAttribute, rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes exposing (class)


white : Color
white =
    rgb255 255 255 255


lightGrey : Color
lightGrey =
    rgb255 155 155 155


dashboardColor : Color
dashboardColor =
    rgb255 228 231 235


grey : Bool -> Element msg
grey checked =
    Element.el
        [ htmlAttribute <| class "focusable"
        , Element.width (Element.px 14)
        , Element.height (Element.px 14)
        , Font.color white
        , Element.centerY
        , Font.size 9
        , Font.center
        , Border.rounded 3
        , Border.color <|
            if checked then
                Element.rgb (59 / 255) (153 / 255) (252 / 255)

            else
                Element.rgb (211 / 255) (211 / 255) (211 / 255)
        , Border.shadow
            { offset = ( 0, 0 )
            , blur = 1
            , size = 1
            , color =
                if checked then
                    Element.rgba (238 / 255) (238 / 255) (238 / 255) 0

                else
                    Element.rgb (238 / 255) (238 / 255) (238 / 255)
            }
        , Background.color <|
            if checked then
                lightGrey

            else
                white
        , Border.width <|
            if checked then
                0

            else
                1
        , Element.inFront
            (Element.el
                [ Border.color white
                , Element.height (Element.px 6)
                , Element.width (Element.px 9)
                , Element.rotate (degrees -45)
                , Element.centerX
                , Element.centerY
                , Element.moveUp 1
                , Element.transparent (not checked)
                , Border.widthEach
                    { top = 0
                    , left = 2
                    , bottom = 2
                    , right = 0
                    }
                ]
                Element.none
            )
        ]
        Element.none
