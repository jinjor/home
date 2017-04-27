port module WebAudioApi exposing (AudioBuffer, decodeAudioData, play, stop)

import Time exposing (..)
import Task exposing (..)
import BinaryDecoder.Byte exposing (ArrayBuffer)
import Json.Encode
import Native.WebAudioApi


type alias Json =
    Json.Encode.Value


type AudioBuffer
    = AudioBuffer Json


type alias Id =
    String


decodeAudioData : ArrayBuffer -> Task String AudioBuffer
decodeAudioData =
    Native.WebAudioApi.decodeAudioData >> Task.map AudioBuffer


port webAudioApiPlay : ( Id, Json, Time ) -> Cmd msg


port webAudioApiStop : Id -> Cmd msg


play : Id -> AudioBuffer -> Time -> Cmd msg
play id (AudioBuffer buffer) time =
    webAudioApiPlay ( id, buffer, time / 1000 )


stop : Id -> Cmd msg
stop id =
    webAudioApiStop id
