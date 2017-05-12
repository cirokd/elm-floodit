module State exposing (init, update)

import Html as H exposing (..)
import Random exposing (..)
import Types exposing (..)


initCols : Int
initCols =
    14


initColors : Int
initColors =
    4


blankGrid : Int -> Grid
blankGrid cols =
    List.indexedMap (,) <| List.repeat (initCols * initCols) 1


init : ( Model, Cmd Msg )
init =
    { cols = initCols
    , colors = initColors
    , grid = blankGrid initCols
    , selectedCols = initCols
    , selectedColors = initColors
    }
        ! [ generateGrid (initCols * initCols) initColors ]


getRandomColorList : Int -> Int -> Generator (List Int)
getRandomColorList colors items =
    Random.list items (Random.int 1 colors)


generateGrid : Int -> Int -> Cmd Msg
generateGrid items colors =
    Random.generate NewGrid (getRandomColorList colors items)


(!!) : List a -> Int -> Maybe a
(!!) xs idx =
    if idx < 0 then
        Nothing
    else
        List.head <| List.drop idx xs
infix 1 !!


replaceInGrid : Grid -> Int -> Int -> Grid
replaceInGrid grid idx colorCode =
    List.append
        (List.take idx grid)
        (( idx, colorCode ) :: List.drop (idx + 1) grid)


getNeighbors : Grid -> Int -> Int -> List GridItem
getNeighbors grid cols idx =
    List.filterMap
        identity
        [ if idx % cols /= 0 then
            grid !! idx - 1
          else
            Nothing
        , grid !! idx - cols
        , if idx % cols /= (cols - 1) then
            grid !! idx + 1
          else
            Nothing
        , grid !! idx + cols
        ]


floodFill : Int -> Int -> Int -> Grid -> List GridItem -> List Int -> Grid
floodFill cols fromColor toColor grid queue seen =
    let
        floodFill_ =
            floodFill cols fromColor toColor
    in
        case queue of
            [] ->
                grid

            ( i, c ) :: rest ->
                if List.member i seen || c /= fromColor then
                    floodFill_ grid rest seen
                else
                    let
                        newGrid =
                            replaceInGrid grid i toColor

                        newQueue =
                            List.append rest <| getNeighbors grid cols i

                        newSeen =
                            i :: seen
                    in
                        floodFill_ newGrid newQueue newSeen


floodFillGrid : Grid -> Int -> Int -> Grid
floodFillGrid grid cols colorCode =
    case List.head grid of
        Just ( i, c ) ->
            floodFill cols c colorCode grid [ ( i, c ) ] []

        Nothing ->
            grid


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []

        FillRandom ->
            model ! [ generateGrid model.colors (List.length model.grid) ]

        NewGrid newGrid ->
            { model | grid = (List.indexedMap (,) <| newGrid) } ! []

        GridItemClicked colorCode ->
            { model | grid = floodFillGrid model.grid model.cols colorCode } ! []

        SizeChanged newCols ->
            { model | selectedCols = Result.withDefault initCols (String.toInt newCols) } ! []

        ColorsChanged newColors ->
            { model | selectedColors = Result.withDefault initColors (String.toInt newColors) } ! []

        NewGame ->
            { model | cols = model.selectedCols, colors = model.selectedColors }
                ! [ generateGrid (model.selectedCols * model.selectedCols) model.selectedColors ]
