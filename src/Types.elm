module Types exposing (..)


type alias GridItem =
    ( Int, Int )


type alias Grid =
    List GridItem


type alias Coords =
    ( Int, Int )


type alias Model =
    { cols : Int
    , colors : Int
    , grid : Grid
    , selectedCols : Int
    , selectedColors : Int
    }


type Msg
    = Noop
    | FillRandom
    | NewGrid (List Int)
    | GridItemClicked Int
    | SizeChanged String
    | ColorsChanged String
    | NewGame
