port module GenJson exposing (..)

import MusicContents
import Json.Encode


type alias Json =
    Json.Encode.Value


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


port response : Json -> Cmd msg


type Model
    = Model


type Msg
    = Msg


init : ( Model, Cmd msg )
init =
    ( Model, response MusicContents.json )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( Model, Cmd.none )
