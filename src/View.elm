module View exposing (view)

import Html as H exposing (..)
import Html.Attributes as HA exposing (style)
import Html.Events as HE exposing (onClick)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE exposing (onClick)
import Types exposing (..)


decodeColor : Int -> String
decodeColor c =
    -- colors from https://flatuicolors.com/
    case c of
        1 ->
            "#c0392b"

        2 ->
            "#27ae60"

        3 ->
            "#2980b9"

        4 ->
            "#f1c40f"

        5 ->
            "#9b59b6"

        6 ->
            "#d35400"

        7 ->
            "#ecf0f1"

        _ ->
            "#34495e"


gridSize : Int
gridSize =
    30


gridSizeMultipliedBy : Int -> String
gridSizeMultipliedBy x =
    toString <| x * gridSize


viewGridItem : Int -> GridItem -> Svg Msg
viewGridItem cols ( idx, colorCode ) =
    S.rect
        [ SA.x <| gridSizeMultipliedBy <| idx % cols
        , SA.y <| gridSizeMultipliedBy <| idx // cols
        , SA.width <| toString gridSize
        , SA.height <| toString gridSize
        , SA.fill <| decodeColor colorCode
        , SE.onClick <| GridItemClicked colorCode
        , SA.style <| "transition: 0.25s all"
        ]
        []


viewGridItems : List GridItem -> Int -> List (Svg Msg)
viewGridItems grid cols =
    List.map (\gridItem -> viewGridItem cols gridItem) grid


viewGrid : Model -> Html Msg
viewGrid model =
    S.svg
        [ SA.width <| gridSizeMultipliedBy model.cols
        , SA.height <| gridSizeMultipliedBy model.cols
        , SA.style <| "border: 5px solid black"
        ]
        (viewGridItems model.grid model.cols)


viewTitle : Html Msg
viewTitle =
    H.h1 [] [ H.text "elm-floodit" ]


viewInfo : Html Msg
viewInfo =
    H.p [] [ H.text "" ]


labelStyle : H.Attribute Msg
labelStyle =
    HA.style
        [ ( "margin-left", "20px" )
        , ( "margin-right", "5px" )
        ]


selectStyle : H.Attribute Msg
selectStyle =
    HA.style
        [ ( "padding", "4px" )
        , ( "font-size", "16px" )
        ]


buttonStyle : H.Attribute Msg
buttonStyle =
    HA.style
        [ ( "font-size", "16px" )
        , ( "padding", "5px" )
        , ( "margin-left", "20px" )
        ]


viewOptions : List Int -> Int -> (String -> String) -> List (Html Msg)
viewOptions values selectedValue display =
    values
        |> List.map toString
        |> List.map
            (\v ->
                H.option
                    [ HA.value v, HA.selected <| toString selectedValue == v ]
                    [ H.text <| display v ]
            )


viewControls : Int -> Int -> Html Msg
viewControls selectedCols selectedColors =
    H.div
        [ HA.style
            [ ( "margin", "15px 0" )
            , ( "font-size", "20px" )
            ]
        ]
        [ H.label [ HA.for "size-select", labelStyle ] [ H.text "Size:" ]
        , H.select
            [ HA.id "size-select", selectStyle, HE.onInput SizeChanged ]
            (viewOptions [ 10, 14, 18, 22, 26 ] selectedCols (\v -> v ++ "x" ++ v))
        , H.label [ HA.for "colors-select", labelStyle ] [ H.text "Colors:" ]
        , H.select
            [ HA.id "colors-select", HE.onInput ColorsChanged, selectStyle ]
            (viewOptions [ 3, 4, 5, 6, 7, 8 ] selectedColors identity)
        , H.button [ buttonStyle, HE.onClick NewGame ] [ H.text "New game" ]
        ]


view : Model -> Html Msg
view model =
    H.div
        [ HA.style
            [ ( "text-align", "center" )
            , ( "font-family", "Courier New, Courier, monospace" )
            ]
        ]
        [ viewTitle
        , viewGrid model
        , viewControls model.selectedCols model.selectedColors
        , viewInfo
        ]
