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
        , "jinjor/elm-debounce"
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
  -- 2017
  [ Content "#little-world" "Little World" (SoundCloud "306090165")
  -- 2016
  , Content "#hokora" "ほこら" (MidiAndMp3 "./contents/music/2016/hokora.mid" "./contents/music/2016/hokora.mp3")
  , Content "#hokora-fc" "ほこら（FCアレンジ by ハイデンさん）" (Mp3 "./contents/music/2016/hokora-fc.mp3")
  , Content "#kira-kira" "Kira Kira" (SoundCloud "278194362")
  , Content "#candy" "Candy" (SoundCloud "240810123")
  -- block
  , Content "#ancient" "ancient" (Mp3 "./contents/music/2016/ancient.mp3")
  , Content "#beach" "beach" (Mp3 "./contents/music/2016/beach.mp3")
  , Content "#cloud" "cloud" (Mp3 "./contents/music/2016/cloud.mp3")
  , Content "#ice" "ice" (Mp3 "./contents/music/2016/ice.mp3")
  , Content "#jungle" "jungle" (Mp3 "./contents/music/2016/jungle.mp3")
  , Content "#kingdom" "kingdom" (Mp3 "./contents/music/2016/kingdom.mp3")
  , Content "#night" "night" (Mp3 "./contents/music/2016/night.mp3")
  , Content "#ninja" "ninja" (Mp3 "./contents/music/2016/ancient.mp3")
  , Content "#volcano" "volcano" (Mp3 "./contents/music/2016/volcano.mp3")
  -- 2015
  , Content "#megalopolis" "Megalopolis" (SoundCloud "236197155")
  , Content "#voice-of-water" "Voice of Water" (SoundCloud "233781385")
  , Content "#wedding-march" "Wedding March" (SoundCloud "228037751")
  , Content "#glass-city" "Glass City" (SoundCloud "200427994")
  -- 2014
  , Content "#summer" "Summer" (MidiAndMp3 "./contents/music/2014/summer.mid" "./contents/music/2014/summer.mp3")
  , Content "#sakura" "桜舞う" (MidiAndMp3 "./contents/music/2014/sakura.mid" "./contents/music/2014/sakura.mp3")
  , Content "#midnight" "真夜中の暇つぶし" (MidiAndMp3 "./contents/music/2014/midnight.mid" "./contents/music/2014/midnight.mp3")
  -- 2013
  , Content "#string" "糸" (MidiAndMp3 "./contents/music/2013/string.mid" "./contents/music/2013/string.mp3")
  , Content "#autumn" "秋風" (MidiAndMp3 "./contents/music/2013/autumn.mid" "./contents/music/2013/autumn.mp3")
  , Content "#afternoon-caos" "午後のカオス" (MidiAndMp3 "./contents/music/2013/afternoon_caos.mid" "./contents/music/2013/afternoon_caos.mp3")
  , Content "#michikusa" "道草" (MidiAndMp3 "./contents/music/2013/michikusa.mid" "./contents/music/2013/michikusa.mp3")
  , Content "#tmp" "Temporary" (MidiAndMp3 "./contents/music/2013/tmp.mid" "./contents/music/2013/tmp.mp3")
  , Content "#hallucination" "幻覚" (MidiAndMp3 "./contents/music/2013/hallucination.mid" "./contents/music/2013/hallucination.mp3")
  , Content "#blue" "BLUE" (MidiAndMp3 "./contents/music/2013/blue.mid" "./contents/music/2013/blue.mp3")
  -- 2012
  , Content "#painter" "変人" (MidiAndMp3 "./contents/music/2012/painter.mid" "./contents/music/2012/painter.mp3")
  , Content "#uploar" "大騒ぎ" (MidiAndMp3 "./contents/music/2012/uploar.mid" "./contents/music/2012/uploar.mp3")
  -- , Content "#air" "" (MidiAndMp3 "./contents/music/2012/air.mid" "./contents/music/2012/air.mp3")
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
  = Mp3 FileName
  | MidiAndMp3 FileName FileName
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
    , main_ [ class "body container" ]
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
      , div [ class "repository" ] ( GitHub.view model.gitHub |> Tuple.second )
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
      Mp3 mp3File ->
        [ title content.hash content.title
        , audio [ class "mp3", src mp3File, controls True ] []
        ]

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
            , div [ class "midi-player-empty", onClick (TriggerLoadFile midiFile) ] [ text "Play" ]
            ]

      SoundCloud id ->
        [ title content.hash content.title
        , div [ class "soundcloud" ] [ soundCloud id ]
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
