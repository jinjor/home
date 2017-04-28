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


decodeAudioData : ArrayBuffer -> Task String AudioBuffer
decodeAudioData =
    Native.WebAudioApi.decodeAudioData >> Task.map AudioBuffer


port webAudioApiPlay : ( Json, Time ) -> Cmd msg


port webAudioApiStop : () -> Cmd msg


play : AudioBuffer -> Time -> Cmd msg
play (AudioBuffer buffer) time =
    webAudioApiPlay ( buffer, time / 1000 )


stop : Cmd msg
stop =
    webAudioApiStop ()
