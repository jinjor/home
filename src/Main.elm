port module Main exposing (..)

import Dict exposing (Dict)
import Task
import Time exposing (Time)
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
import GitHub exposing (GitHub)


port start : () -> Cmd msg
port stop : () -> Cmd msg


main : Program Never Model Msg
main =
  program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = lazy view
    }


type alias Model =
  { midiContents : Dict String MidiContent
  , playing : Maybe String
  , startTime : Time
  , currentTime : Time
  , futureNotes : List (Detailed Note)
  , selectedMidiOut : Maybe String
  , showConfig : Bool
  , gitHub : GitHub
  , error : Error
  }


type Error
  = NoError
  | DecodeError ArrayBuffer Byte.Error


type alias FileName
  = String


type Msg
  = TriggerLoadFile FileName
  | ReadBuffer FileName (Result String ArrayBuffer)
  | Back
  | TriggerStart FileName Midi
  | Start FileName Midi Time
  | Stop
  | Tick Time
  | ToggleTrack FileName Int
  | ToggleConfig
  | GitHubMsg GitHub.Msg


init : (Model, Cmd Msg)
init =
  let
    (gitHub, cmd) =
      GitHub.init GitHubMsg (Just "jinjor")
        [ "jinjor/elm-diff"
        , "jinjor/elm-time-travel"
        , "jinjor/elm-html-parser"
        , "jinjor/elm-contextmenu"
        , "jinjor/elm-inline-hover"
        , "WorksApplications/office-maker"
        ]
  in
    ( Model initialMidiCountents Nothing 0 0 [] Nothing False gitHub NoError
    , cmd
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
    TriggerLoadFile fileName ->
      ( model
      , File.fetchArrayBuffer fileName
          |> Task.attempt ( ReadBuffer fileName )
      )

    ReadBuffer fileName (Ok buf) ->
      case Byte.decode SmfDecoder.smf buf of
        Ok smf ->
          ( { model
              | midiContents =
                  model.midiContents
                    |> Dict.update fileName ( Maybe.map (\midiContent ->
                      { midiContent | midi = Just (Midi.fromSmf smf) }
                    ))
            }
          , Cmd.none
          )

        Err e ->
          ({ model
             | error = DecodeError buf e
          }, Cmd.none)

    ReadBuffer fileName (Err s) ->
      Debug.crash ("failed to read arrayBuffer of file '" ++ fileName ++ "': " ++ s)

    Back ->
      ({ model
          | startTime = 0
          , currentTime = 0
          , playing = Nothing
      }, Cmd.none )

    TriggerStart fileName midi ->
      ( model
      , Time.now
          |> Task.perform ( Start fileName midi )
      )

    Start fileName midi currentTime ->
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
            , playing = Just fileName
            , futureNotes = prepareFutureNotes (currentTime - startTime) midi
          }
        , start ()
        )
          |> andThen (update (Tick currentTime))

    Stop ->
      ( { model
          | playing = Nothing
        }
      , stop ()
      )

    Tick currentTime ->
      ({ model
          | currentTime = currentTime
      }, Cmd.none )

    ToggleTrack fileName index ->
      ( { model
          | midiContents =
              model.midiContents
                |> Dict.update fileName ( Maybe.map (\midiContent ->
                  { midiContent | midi = Maybe.map (Midi.toggleVisibility index) midiContent.midi }
                ))
        }
      , Cmd.none
      )

    ToggleConfig ->
      ( { model | showConfig = not model.showConfig }
      , Cmd.none
      )

    GitHubMsg msg ->
      ( { model | gitHub = GitHub.update msg model.gitHub }
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
    [ if model.playing /= Nothing then
        Time.every (1000 * Time.millisecond / 30) Tick
      else
        Sub.none
    ]


contents : List Content
contents =
  [ Content "#summer" "Summer" (MidiAndMp3 "./assets/sample.mid" "")
  , Content "#little-world" "Little World" (SoundCloud "306090165")
  , Content "#kira-kira" "Kira Kira" (SoundCloud "278194362")
  , Content "#candy" "Candy" (SoundCloud "240810123")
  ]


initialMidiCountents : Dict String MidiContent
initialMidiCountents =
  contents
    |> List.filterMap (\content ->
      case content.details of
        MidiAndMp3 midiFile mp3File  ->
          Just (midiFile, MidiContent midiFile mp3File False Nothing)

        _ ->
          Nothing
      )
    |> Dict.fromList


type alias Content =
  { hash : String
  , title : String
  , details : Details
  }


type Details
  = MidiAndMp3 FileName FileName
  | SoundCloud String


type alias MidiContent =
  { midiFile : String
  , mp3File : String
  , opened : Bool
  , midi : Maybe Midi
  }


view : Model -> Html Msg
view model =
  div
    []
    [ header [ class "header" ]
        [ div
            [ class "container" ]
            [ h1 [] [ text "World Maker" ] ]
        ]
    , body [ class "body container" ]
      [ p [] [ text "ジンジャー と Yosuke Torii のホームページ" ]
      , h2 [] [ Shape.note, text "Music" ]
      , p [] [ text "世界を創る音楽" ]
      , ul [] ( List.map ( viewContent model ) contents )
      , case model.error of
          NoError ->
            text ""

          DecodeError buf e ->
            text (toString e)
      , h2 [] [ Shape.note, text "Development" ]
      , p [] [ text "プログラミングは芸術" ]
      , div [] ( GitHub.view model.gitHub |> Tuple.second )
      , h2 [] [ Shape.note, text "Paintings" ]
      , p [] [ text "ペイントでお絵かき" ]
      , div [ class "paintings-container paintings-container-single" ]
          [ div [] [ img [ class "paintings-image", src "./contents/paintings/trip.png" ] [] ]
          ]
      , div [ class "paintings-container" ]
          [ div [] [ img [ class "paintings-image", src "./contents/paintings/cafe.png" ] [] ]
          ]
      , div [ class "paintings-container" ]
          [ div [] [ img [ class "paintings-image", src "./contents/paintings/rain.png" ] [] ]
          ]
      , div [ class "paintings-container" ]
          [ div [] [ img [ class "paintings-image", src "./contents/paintings/hanabi.png" ] [] ]
          , div [] [ img [ class "paintings-image", src "./contents/paintings/totoro.png" ] [] ]
          ]
      , div [ class "paintings-container paintings-container-small" ]
          [ div [] [ img [ class "paintings-image", src "./contents/paintings/clock.png" ] [] ]
          , div [] [ img [ class "paintings-image", src "./contents/paintings/strong-zero.png" ] [] ]
          , div [] [ img [ class "paintings-image", src "./contents/paintings/orange.png" ] [] ]
          ]
      , h2 [] [ Shape.note, text "Links" ]
      , p [] [ text "" ]
      , ul []
          [ li [] [ a [ href "https://soundcloud.com/jinjor" ] [ text "SoundCloud" ] ]
          , li [] [ a [ href "https://github.com/jinjor" ] [ text "GitHub" ] ]
          , li [] [ a [ href "https://twitter.com/jinjor" ] [ text "Twitter" ] ]
          , li [] [ a [ href "http://jinjor-labo.hatenablog.com/" ] [ text "Blog" ] ]
          ]
      ]
    ]


viewContent : Model -> Content -> Html Msg
viewContent model content =
  li [ class "contents-item" ] <|
    case content.details of
      MidiAndMp3 midiFile mp3File ->
        case Dict.get midiFile model.midiContents |> Maybe.andThen .midi of
          Just midi ->
            [ title content.hash content.title
            , MidiPlayer.view
                { onBack = Back
                , onStart = TriggerStart midiFile midi
                , onStop = Stop
                , onToggleTrack = ToggleTrack midiFile
                }
                ( model.playing == Just midiFile )
                ( model.currentTime - model.startTime )
                midi
            ]

          Nothing ->
            [ title content.hash content.title
            , button [ onClick (TriggerLoadFile midiFile) ] [ text "Play" ]
            ]

      SoundCloud id ->
        [ title content.hash content.title
        , soundCloud id
        ]


title : String -> String -> Html msg
title hash s =
  a [ class "contents-item-link", href hash ] [ text s ]


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
