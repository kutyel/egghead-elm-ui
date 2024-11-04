module Main exposing (main)

import Browser exposing (element)
import Browser.Events exposing (onResize)
import Checkbox exposing (dashboardColor, lightGrey, white)
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)



---- MODEL ----


type Menu
    = Overall
    | Category1
    | Category2
    | Category3


type alias Model =
    { device : Device
    , selected : List ( Menu, String )
    , overallDropdownState : Dropdown.State String
    , category1DropdownState : Dropdown.State String
    , category2DropdownState : Dropdown.State String
    , category3DropdownState : Dropdown.State String
    }


init : ( Int, Int ) -> ( Model, Cmd Msg )
init ( h, w ) =
    ( { device = classifyDevice { height = h, width = w }
      , selected = []
      , overallDropdownState = Dropdown.init "overall"
      , category1DropdownState = Dropdown.init "category1"
      , category2DropdownState = Dropdown.init "category2"
      , category3DropdownState = Dropdown.init "category3"
      }
    , Cmd.none
    )


options : List String
options =
    List.range 1 10 |> List.map (\item -> "Option " ++ String.fromInt item)


getState : Model -> Menu -> Dropdown.State String
getState model menu =
    case menu of
        Overall ->
            model.overallDropdownState

        Category1 ->
            model.category1DropdownState

        Category2 ->
            model.category2DropdownState

        Category3 ->
            model.category3DropdownState



---- UPDATE ----


type Msg
    = NoOp Bool
    | ResizedApp Int Int
    | OptionPicked Menu (List String)
    | DropdownMsg Menu (Dropdown.Msg String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp _ ->
            ( model, Cmd.none )

        ResizedApp w h ->
            ( { model | device = classifyDevice { height = h, width = w } }, Cmd.none )

        OptionPicked menu selections ->
            let
                subset =
                    List.filter (\( m, _ ) -> m /= menu) model.selected
            in
            ( { model
                | selected =
                    selections
                        |> List.map (\option -> ( menu, option ))
                        |> List.append subset
              }
            , Cmd.none
            )

        DropdownMsg menu subMsg ->
            case menu of
                Overall ->
                    let
                        ( state, cmd ) =
                            Dropdown.update (dropdownConfig model menu) subMsg model model.overallDropdownState
                    in
                    ( { model | overallDropdownState = state }, cmd )

                Category1 ->
                    let
                        ( state, cmd ) =
                            Dropdown.update (dropdownConfig model menu) subMsg model model.category1DropdownState
                    in
                    ( { model | category1DropdownState = state }, cmd )

                Category2 ->
                    let
                        ( state, cmd ) =
                            Dropdown.update (dropdownConfig model menu) subMsg model model.category2DropdownState
                    in
                    ( { model | category2DropdownState = state }, cmd )

                Category3 ->
                    let
                        ( state, cmd ) =
                            Dropdown.update (dropdownConfig model menu) subMsg model model.category3DropdownState
                    in
                    ( { model | category3DropdownState = state }, cmd )



---- VIEW ----


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


dropdownConfig : Model -> Menu -> Dropdown.Config String Msg Model
dropdownConfig model menu =
    let
        arrow icon =
            el [ Font.size 7, paddingEach { edges | left = 5 } ] icon

        selectionFromModel m =
            m.selected
                |> List.filter (\( menuOption, _ ) -> menuOption == menu)
                |> List.map Tuple.second

        itemToElement _ _ item =
            Input.checkbox [ Element.focused [] ]
                { onChange = NoOp
                , icon = Checkbox.grey
                , checked = List.any ((==) ( menu, item )) model.selected
                , label = Input.labelRight [ paddingEach { edges | left = 7 } ] <| text item
                }

        selectAttrs =
            [ pointer
            , paddingXY 13 7
            , Background.color (rgb255 224 228 237)
            , Border.rounded 15
            , Font.letterSpacing 1
            , Font.size 16
            , Element.focused
                [ Background.color (rgb255 25 45 91), Font.color white ]
            ]

        listAttrs =
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
    in
    Dropdown.multi
        { itemsFromModel = always options
        , selectionFromModel = selectionFromModel
        , dropdownMsg = DropdownMsg menu
        , onSelectMsg = OptionPicked menu
        , itemsToPrompt = always btn
        , itemToElement = itemToElement
        }
        |> Dropdown.withPromptElement btn
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withOpenCloseButtons { openButton = arrow (text "▼"), closeButton = arrow (text "▲") }


dashboard : Model -> Element Msg
dashboard model =
    let
        attrs =
            [ centerX
            , centerY
            , spacing 15
            , Border.rounded 15
            , Background.color dashboardColor
            ]

        overall =
            row [ width fill ]
                [ card "OVERALL"
                    Overall
                    model
                    [ height <| px 300, Border.shadow { offset = ( 0, 3 ), size = 0.1, blur = 5, color = lightGrey } ]
                ]

        breakdown =
            row
                [ width fill
                , paddingEach { edges | left = 20, top = 30 }
                , Region.heading 2
                , Font.semiBold
                , Font.size 18
                ]
                [ text "BREAKDOWN" ]

        subtitle =
            paragraph [ paddingEach { edges | left = 20 }, Font.color lightGrey ]
                [ text "Select the options from the dropdown menus" ]

        category1 =
            card "CATEGORY 1" Category1 model []

        category2 =
            card "CATEGORY 2" Category2 model [ height <| px 300 ]

        category3 =
            card "CATEGORY 3" Category3 model [ height <| px 300 ]
    in
    case model.device.class of
        Phone ->
            column attrs
                [ overall
                , breakdown
                , subtitle
                , category1
                , category2
                , category3
                ]

        _ ->
            column (attrs ++ [ width (px 800), Font.size 16 ])
                [ overall
                , breakdown
                , subtitle
                , row [ width fill, paddingXY 20 0 ] [ category1 ]
                , row [ width fill, paddingEach { top = 0, right = 20, bottom = 20, left = 20 }, spacing 15 ]
                    [ column [ width fill ] [ category2 ]
                    , column [ width fill ] [ category3 ]
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


card : String -> Menu -> Model -> List (Attribute Msg) -> Element Msg
card title menu model attrs =
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
            [ el [ Region.heading 3, Font.semiBold, Font.size 18 ] (text title)
            , paragraph
                -- we need to use `paragraph` here because `el` or `text` does not wrap by default!
                [ Font.color lightGrey ]
                [ text <|
                    case List.filter (\( m, _ ) -> m == menu) model.selected of
                        [] ->
                            ""

                        xs ->
                            xs |> List.reverse |> List.map Tuple.second |> String.join ", " |> String.append "Selected: "
                ]
            ]
        , el [ alignTop, alignRight ] <|
            Dropdown.view
                (dropdownConfig model menu)
                model
                (getState model menu)
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize ResizedApp
        , Dropdown.onOutsideClick model.overallDropdownState (DropdownMsg Overall)
        , Dropdown.onOutsideClick model.category1DropdownState (DropdownMsg Category1)
        , Dropdown.onOutsideClick model.category2DropdownState (DropdownMsg Category2)
        , Dropdown.onOutsideClick model.category3DropdownState (DropdownMsg Category3)
        ]



---- PROGRAM ----


main : Program ( Int, Int ) Model Msg
main =
    element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
