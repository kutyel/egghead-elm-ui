module Main exposing (main)

import Browser exposing (sandbox)
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
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

        DropdownMsg (Dropdown.OnSelect str) ->
            { model | selectedOption = Just str }

        DropdownMsg subMsg ->
            -- TODO: fix this
            let
                ( state, cmd ) =
                    Dropdown.update dropdownConfig subMsg model.dropdownState options
            in
            { model | dropdownState = state }



---- VIEW ----


white : Color
white =
    rgb255 255 255 255


lightGrey : Color
lightGrey =
    rgb255 155 155 155


view : Model -> Html Msg
view model =
    Element.layout [ Background.color (rgb255 238 241 245) ] <| dashboard model


edges : { top : number, right : number, bottom : number, left : number }
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


dropdownConfig : Dropdown.Config String Msg
dropdownConfig =
    let
        arrow icon =
            el [ Font.size 7, paddingEach { edges | left = 5 } ] icon

        itemToElement selected highlighted item =
            Input.checkbox []
                { onChange = ChechboxChecked
                , icon = Input.defaultCheckbox -- TODO: customise gray checkbox? 🤔
                , checked = False -- TODO: adjust to state here
                , label = Input.labelRight [ paddingEach { edges | left = 7 } ] <| text item
                }
    in
    Dropdown.Config
        { closeButton = arrow (text "▲")
        , containerAttributes = []
        , dropdownMsg = DropdownMsg
        , dropdownType = Dropdown.Basic
        , filterPlaceholder = Nothing
        , itemToElement = itemToElement
        , itemToPrompt = always btn
        , itemToText = \_ -> ""
        , listAttributes =
            [ Background.color white
            , Border.rounded 5
            , padding 20
            , spacing 20
            , alignRight
            , height (px 220)
            , scrollbarX
            , Border.shadow
                { offset = ( 0, 4 )
                , size = 0.1
                , blur = 6
                , color = lightGrey
                }
            ]
        , onSelectMsg = OptionPicked
        , openButton = arrow (text "▼")
        , promptElement = el [ width fill ] btn
        , searchAttributes = []
        , selectAttributes =
            [ pointer
            , paddingXY 13 7
            , Background.color (rgb255 224 228 237)
            , Border.rounded 15
            , Font.letterSpacing 1
            , Font.size 16
            , Element.focused
                [ Background.color (rgb255 25 45 91), Font.color white ]
            ]
        }


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
                    , color = lightGrey
                    }
                ]
            ]
        , row
            [ width fill
            , paddingEach { edges | left = 20, top = 30 }
            , Region.heading 2
            , Font.semiBold
            ]
            [ text "BREAKDOWN" ]
        , paragraph
            [ paddingEach { edges | left = 20 }
            , Font.color lightGrey
            ]
            [ text "Select the options from dropdown menu." ]
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
        , Element.focused [ Background.color (rgb255 25 45 91), Font.color white ]
        ]
        { label = el [] (text "COMPARE")
        , onPress = Nothing
        }


card : String -> Model -> List (Attribute Msg) -> Element Msg
card title model attrs =
    row
        ([ Background.color white
         , Border.rounded 15
         , Font.color (rgb255 51 51 51)
         , height <| px 200
         , padding 20
         , width fill
         ]
            ++ attrs
        )
        [ textColumn [ alignTop, alignLeft, width fill, spacing 10 ]
            [ el [ Region.heading 3, Font.semiBold ] (text title)

            -- we need to use `paragraph` here because `el` or `text` does not wrap by default!!
            , paragraph [ Font.color lightGrey ] [ text <| Maybe.withDefault "" model.selectedOption ]
            ]
        , el [ alignTop, alignRight ] <| Dropdown.view dropdownConfig model.dropdownState options
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    sandbox { view = view, init = init, update = update }
