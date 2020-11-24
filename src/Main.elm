module Main exposing (main)

import Browser exposing (sandbox)
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



---- MODEL ----


type alias Model =
    { dropdownState : Dropdown.State String
    , selectedOption : Maybe String
    }


init : Model
init =
    { dropdownState = Dropdown.init "dropdown"
    , selectedOption = Nothing
    }


options : List String
options =
    List.range 1 10 |> List.map (\item -> "Option " ++ String.fromInt item)



---- UPDATE ----


type Msg
    = OptionPicked (Maybe String)
    | DropdownMsg (Dropdown.Msg String)
    | ChechboxChecked Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChechboxChecked _ ->
            model

        OptionPicked option ->
            { model | selectedOption = option }

        DropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update dropdownConfig subMsg model.dropdownState options
            in
            { model | dropdownState = state }



---- VIEW ----


white : Color
white =
    rgb255 255 255 255


view : Model -> Html Msg
view model =
    Element.layout [ Background.color (rgb255 238 241 245) ] <| dashboard model


dropdownConfig : Dropdown.Config String Msg
dropdownConfig =
    let
        itemToElement selected highlighted item =
            Input.checkbox []
                { onChange = ChechboxChecked
                , icon = Input.defaultCheckbox
                , checked = False -- TODO: adjust to state here
                , label = Input.labelRight [] <| text item
                }
    in
    Dropdown.basic DropdownMsg OptionPicked (always btn) itemToElement


dashboard : Model -> Element Msg
dashboard model =
    column
        [ Background.color (rgb255 228 231 235)
        , centerX
        , centerY
        , spacing 15
        , Border.rounded 15
        , width (px 1000)
        ]
        [ row [ width fill ]
            [ card "OVERALL"
                model
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
        , row [ width fill, paddingXY 20 0 ] [ card "CATEGORY 1" model [] ]
        , row [ width fill, paddingEach { top = 0, right = 20, bottom = 20, left = 20 }, spacing 15 ]
            [ column [ width fill ] [ card "CATEGORY 2" model [ height <| px 350 ] ]
            , column [ width fill ] [ card "CATEGORY 3" model [ height <| px 350 ] ]
            ]
        ]


btn : Element a
btn =
    Input.button
        [ alignTop
        , alignRight
        , paddingXY 13 7
        , Background.color (rgb255 224 228 237)
        , Border.rounded 15
        , Font.medium
        , Font.letterSpacing 1
        , Font.size 16
        , Element.focused
            [ Background.color (rgb255 25 45 91), Font.color white ]
        ]
        { label =
            row []
                [ text "COMPARE"
                , el [ Font.size 7 ] (text "  ▲▼")
                ]
        , onPress = Nothing
        }


card : String -> Model -> List (Attribute Msg) -> Element Msg
card title model attrs =
    row
        ([ Background.color white
         , Border.rounded 15
         , Font.bold
         , Font.color (rgb255 51 51 51)
         , height <| px 200
         , padding 20
         , width fill
         ]
            ++ attrs
        )
        [ el [ alignTop, alignLeft ] (text title)
        , el [ alignTop, alignRight ] <| Dropdown.view dropdownConfig model.dropdownState options
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    sandbox { view = view, init = init, update = update }
