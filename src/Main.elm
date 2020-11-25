module Main exposing (main)

import Browser exposing (sandbox)
import Checkbox exposing (lightGrey, white)
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
    { open : Maybe Menu
    , dropdownState : Dropdown.State String
    , selected : List ( Menu, String )
    }


init : Model
init =
    { dropdownState = Dropdown.init "dropdown"
    , selected = []
    , open = Nothing
    }


options : List String
options =
    List.range 1 10 |> List.map (\item -> "Option " ++ String.fromInt item)



---- UPDATE ----


type Msg
    = ToggleMenu Menu
    | OptionPicked Menu (Maybe String)
    | ChechboxClicked Menu String Bool
    | DropdownMsg Menu (Dropdown.Msg String)


update : Msg -> Model -> Model
update msg model =
    case msg of
        OptionPicked _ _ ->
            -- TODO: maybe remove this?
            model

        ToggleMenu menu ->
            case model.open of
                Nothing ->
                    { model | open = Just menu }

                Just m ->
                    if m == menu then
                        { model | open = Nothing }

                    else
                        { model | open = Just menu }

        ChechboxClicked menu str checked ->
            if List.any ((==) ( menu, str )) model.selected then
                { model | selected = List.filter ((/=) ( menu, str )) model.selected }

            else
                { model | selected = ( menu, str ) :: model.selected }

        -- TODO: handle manuall which dropdown should stay open!
        --
        -- DropdownMsg menu Dropdown.OnClickPrompt ->
        --     case model.open of
        --         Nothing ->
        --             { model | open = Just menu }
        --         Just m ->
        --             if m == menu then
        --                 { model | open = Nothing }
        --             else
        --                 { model | open = Just menu }
        DropdownMsg menu subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update (dropdownConfig menu model) subMsg model.dropdownState options
            in
            { model | dropdownState = state }



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


dropdownConfig : Menu -> Model -> Dropdown.Config String Msg
dropdownConfig menu model =
    let
        arrow icon =
            el [ Font.size 7, paddingEach { edges | left = 5 } ] icon

        itemToElement selected highlighted item =
            Input.checkbox []
                { onChange = ChechboxClicked menu item
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
    Dropdown.basic (DropdownMsg menu) (OptionPicked menu) (always btn) itemToElement
        |> Dropdown.withPromptElement btn
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withOpenCloseButtons { openButton = arrow (text "▼"), closeButton = arrow (text "▲") }


dashboard : Model -> Element Msg
dashboard model =
    -- FIXME: make this tiny bit reponsive to add one more lesson to the course! 💪🏼
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
                Overall
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
        , row [ width fill, paddingXY 20 0 ] [ card "CATEGORY 1" Category1 model [] ]
        , row [ width fill, paddingEach { top = 0, right = 20, bottom = 20, left = 20 }, spacing 15 ]
            [ column [ width fill ] [ card "CATEGORY 2" Category2 model [ height <| px 350 ] ]
            , column [ width fill ] [ card "CATEGORY 3" Category3 model [ height <| px 350 ] ]
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
            [ el [ Region.heading 3, Font.semiBold ] (text title)

            -- we need to use `paragraph` here because `el` or `text` does not wrap by default!
            , paragraph
                [ Font.color lightGrey ]
                [ text <|
                    case List.filter (\( m, _ ) -> m == menu) model.selected of
                        [] ->
                            ""

                        xs ->
                            xs
                                |> List.reverse
                                |> List.map Tuple.second
                                |> String.join ", "
                                |> String.append "Selected: "
                ]
            ]
        , el [ alignTop, alignRight ] <| Dropdown.view (dropdownConfig menu model) model.dropdownState options
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    sandbox { view = view, init = init, update = update }
