module Main exposing (main)

import Browser exposing (sandbox)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)



---- MODEL ----


type alias Model =
    {}


init : Model
init =
    {}



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [ Background.color (rgb255 238 241 245) ] dashboard


dashboard : Element msg
dashboard =
    column
        [ Background.color (rgb255 228 231 235)
        , centerX
        , centerY
        , spacing 15
        , Border.rounded 15
        , width (px 1000)
        ]
        [ row [ width fill ]
            [ card "overall"
                [ height <| px 350
                , Border.shadow
                    { offset = ( 0, 3 )
                    , size = 0.1
                    , blur = 5
                    , color = rgb255 100 100 100
                    }
                ]
            ]
        , row [ width fill, padding 20 ] [ text "BREAKDOWN" ]
        , row [ width fill, paddingXY 20 0 ] [ card "category 1" [] ]
        , row [ width fill, paddingEach { top = 0, right = 20, bottom = 20, left = 20 }, spacing 15 ]
            [ column [ width fill ] [ card "category 2" [ height <| px 350 ] ]
            , column [ width fill ] [ card "category 3" [ height <| px 350 ] ]
            ]
        ]


card : String -> List (Attribute msg) -> Element msg
card title attrs =
    el
        ([ Background.color (rgb255 255 255 255)
         , Border.rounded 15
         , Font.alignLeft
         , Font.bold
         , Font.color (rgb255 51 51 51)
         , height <| px 200
         , padding 20
         , width fill
         ]
            ++ attrs
        )
        (text <| String.toUpper title)



---- PROGRAM ----


main : Program () Model Msg
main =
    sandbox { view = view, init = init, update = update }
