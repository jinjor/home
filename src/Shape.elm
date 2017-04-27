module Shape exposing (..)

import Html exposing (..)
import Svg.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


note : Svg msg
note =
    svg
        [ width "36", height "36", class "shape-note" ]
        [ Svg.path [ d "M16,6L17,30L20,30L18,8zM17,12L29,15L29,12L16,6zM18,30L15,30z" ] []
        , circle [ cx "15", cy "29", r "5" ] []
        ]
