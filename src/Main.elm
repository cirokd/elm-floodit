module Main exposing (main)

import Html as H exposing (program)
import State exposing (init, update)
import View exposing (view)


main =
    H.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
