port module Main exposing (..)

import Array
import Json.Decode as Decode
import Task
import Time exposing (Time)
import Process
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Shape
import BinaryDecoder.File as File exposing (File)
import BinaryDecoder.Byte as Byte exposing (ArrayBuffer, Error)
import SmfDecoder exposing (Smf)
import Midi exposing (Midi, Note, Detailed)
import MidiPlayer


port start : () -> Cmd msg
port stop : () -> Cmd msg


main : Program (Maybe String) Model Msg
main =
  programWithFlags
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = lazy view
    }


type alias Model =
  { midi : Maybe Midi
  , playing : Bool
  , startTime : Time
  , currentTime : Time
  , futureNotes : List (Detailed Note)
  , selectedMidiOut : Maybe String
  , showConfig : Bool
  , error : Error
  }


type Error
  = NoError
  | DecodeError ArrayBuffer Byte.Error


get : (data -> Maybe a) -> data -> a
get f data =
  case f data of
    Just a ->
      a

    Nothing ->
      Debug.crash "undefined"


see : (data -> Maybe a) -> data -> b -> b
see f data b =
  case f data of
    Just a ->
      b

    Nothing ->
      Debug.crash "undefined"


type Msg
  = GotFile File
  | LoadMidi
  | ReadBuffer (Result String ArrayBuffer)
  | Back
  | Start Time
  | Stop
  | Tick Time
  | Timed (Time -> Msg)
  | ToggleTrack Int
  | ToggleConfig


init : Maybe String -> (Model, Cmd Msg)
init midiFile =
  ( Model Nothing False 0 0 [] Nothing False NoError
  , midiFile
      |> Maybe.map (\file -> Task.attempt ReadBuffer (File.fetchArrayBuffer file))
      |> Maybe.withDefault Cmd.none
  )


andThen : (model -> (model, Cmd msg)) -> (model, Cmd msg) -> (model, Cmd msg)
andThen f (model, cmd) =
  let
    (newModel, newCmd) =
      f model
  in
    newModel ! [ cmd, newCmd ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotFile file ->
      ( model
      , Task.attempt ReadBuffer (File.readFileAsArrayBuffer file |> Task.mapError toString)
      )

    LoadMidi ->
      ( model
      , Task.attempt ReadBuffer (File.fetchArrayBuffer "sample.mid")
      )

    ReadBuffer (Ok buf) ->
      case Byte.decode SmfDecoder.smf buf of
        Ok smf ->
          ( { model
              | midi = Just (Midi.fromSmf smf)
            }
          , Cmd.none
          )

        Err e ->
          ({ model
             | error = DecodeError buf e
          }, Cmd.none)

    ReadBuffer (Err s) ->
      Debug.crash ("failed to read arrayBuffer: " ++ s)

    Back ->
      ({ model
          | startTime = 0
          , currentTime = 0
          , playing = False
      }, Cmd.none )

    Start currentTime ->
      let
        startTime =
          if model.currentTime > 0 then
            currentTime - (model.currentTime - model.startTime)
          else
            currentTime
      in
        ( { model
            | startTime = startTime
            , currentTime = currentTime
            , playing = True
            , futureNotes = prepareFutureNotes (currentTime - startTime) (get .midi model)
          }
        , start ()
        )
          |> andThen (update (Tick currentTime))

    Stop ->
      ( { model
          | playing = False
        }
      , stop ()
      )

    Tick currentTime ->
      ({ model
          | currentTime = currentTime
      }, Cmd.none )

    Timed toMsg ->
      ( model, Task.perform toMsg Time.now )

    ToggleTrack index ->
      ( { model | midi = Just (Midi.toggleVisibility index <| get .midi model) }
      , Cmd.none
      )

    ToggleConfig ->
      ( { model | showConfig = not model.showConfig }
      , Cmd.none
      )


prepareFutureNotes : Time -> Midi -> List (Detailed Note)
prepareFutureNotes time midi =
  midi.tracks
    |> List.indexedMap (,)
    |> List.concatMap (\(index, track) -> List.map (Midi.addDetails index track.channel) track.notes )
    |> List.sortBy .position
    |> dropWhile (\note -> Midi.positionToTime midi.timeBase note.position < time)


dropWhile : (a -> Bool) -> List a -> List a
dropWhile f list =
  case list of
    [] -> []
    x :: xs ->
      if f x then
        dropWhile f xs
      else
        list


splitWhile : (a -> Bool) -> List a -> List a -> (List a, List a)
splitWhile f taken list =
  case list of
    [] ->
      (taken, [])

    x :: xs ->
      if f x then
        splitWhile f (x :: taken) xs
      else
        (taken, list)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if model.playing then
        Time.every (1000 * Time.millisecond / 30) Tick
      else
        Sub.none
    ]


view : Model -> Html Msg
view model =
  div []
  [ header [ class "header" ]
      [ div
          [ class "container" ]
          [ h1 [] [ text "World Maker" ] ]
      ]
  , body [ class "body container" ]
    [ h2 [] [ Shape.note, text "Music" ]
    , ul []
        [ li
            [ class "contents-item" ]
            [ title "#summer" "Summer"
            , midi model
            ]
        , li
            [ class "contents-item" ]
            [ title "#little-world" "Little World"
            , soundCloud "306090165"
            ]
        , li
            [ class "contents-item" ]
            [ title "#kira-kira" "Kira Kira"
            , soundCloud "278194362"
            ]
        , li
            [ class "contents-item" ]
            [ title "#candy" "Candy"
            , soundCloud "240810123"
            ]
        , li
            [ class "contents-item" ]
            [ title "#little-world" "Little World"
            , soundCloud "306090165"
            ]
        , li
            [ class "contents-item" ]
            [ title "#kira-kira" "Kira Kira"
            , soundCloud "278194362"
            ]
        , li
            [ class "contents-item" ]
            [ title "#candy" "Candy"
            , soundCloud "240810123"
            ]
        ]
    , case model.error of
        NoError ->
          text ""

        DecodeError buf e ->
          text (toString e)
    ]
  ]

title : String -> String -> Html msg
title hash s =
  a [ class "contents-item-link", href hash ] [ text s ]


midi : Model -> Html Msg
midi model =
  case model.midi of
    Just midi ->
      MidiPlayer.view
        { onBack = Back
        , onStart = Timed Start
        , onStop = Stop
        , onToggleTrack = ToggleTrack
        }
        model.playing
        (model.currentTime - model.startTime)
        midi

    _ ->
      text ""


soundCloud : String -> Html msg
soundCloud id =
  iframe
    [ attribute "width" "100%"
    , attribute "height" "20"
    , attribute "scrolling" "no"
    , attribute "frameborder" "no"
    , src <|
        "https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/" ++ id ++
        "&amp;color=ff5500&amp;inverse=false&amp;auto_play=false&amp;show_user=true"
    ]
    []
