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
import WebAudioApi exposing (AudioBuffer)
import Markdown
import Navigation exposing (Location)
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Navigation.program
        (\location -> NoOp)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = lazy view
        }


type alias Model =
    { midiContents : Dict String MidiContent
    , selected : Maybe Content
    , playing : Bool
    , startTime : Time
    , currentTime : Time
    , futureNotes : List (Detailed Note)
    , selectedMidiOut : Maybe String
    , showConfig : Bool
    , gitHub : GitHub
    , fullscreen : Bool
    , error : Error
    }


type Error
    = NoError
    | DecodeError ArrayBuffer Byte.Error


type alias FileName =
    String


type Msg
    = NoOp
    | OpenPlayer Bool Content
    | TriggerLoadMidiAndMp3 Bool FileName FileName
    | LoadedMidiAndMp3 Bool FileName FileName (Result String ( AudioBuffer, ArrayBuffer ))
    | Back
    | TriggerStart Midi AudioBuffer
    | Start Midi AudioBuffer Time
    | Stop
    | Fullscreen Bool
    | Close
    | Tick Time
    | ToggleConfig
    | GitHubMsg GitHub.Msg


port moveToCard : String -> Cmd msg


init : Location -> ( Model, Cmd Msg )
init location =
    GitHub.init GitHubMsg
        (Just "jinjor")
        [ "jinjor/elm-diff"
        , "jinjor/elm-time-travel"
        , "jinjor/elm-html-parser"
        , "jinjor/elm-contextmenu"
        , "jinjor/elm-inline-hover"
        , "jinjor/elm-debounce"
        ]
        |> andThen
            (\gitHub ->
                let
                    model =
                        Model initialMidiCountents Nothing False 0 0 [] Nothing False gitHub False NoError

                    id =
                        String.dropLeft 1 location.hash

                    firstContent =
                        contents
                            |> List.filter (\content -> content.hash == id)
                            |> List.head
                in
                    case firstContent of
                        Just content ->
                            update (OpenPlayer True content) model
                                |> andThen (\model -> ( model, moveToCard id ))

                        Nothing ->
                            ( model, Cmd.none )
            )


andThen : (model1 -> ( model2, Cmd msg )) -> ( model1, Cmd msg ) -> ( model2, Cmd msg )
andThen f ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            f model
    in
        newModel ! [ cmd, newCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OpenPlayer isInitialLoad content ->
            model.selected
                |> Maybe.map (\_ -> update Stop model)
                |> Maybe.withDefault ( model, Cmd.none )
                |> andThen (update Back)
                |> andThen
                    (if isInitialLoad then
                        update (Fullscreen True)
                     else
                        flip (,) Cmd.none
                    )
                |> andThen (\model -> ( { model | selected = Just content }, Cmd.none ))
                |> andThen
                    (\model ->
                        case content.details of
                            Mp3 mp3File ->
                                ( model, Cmd.none )

                            MidiAndMp3 midiFile mp3File delay ->
                                update (TriggerLoadMidiAndMp3 isInitialLoad midiFile mp3File) model

                            SoundCloud id ->
                                ( model, Cmd.none )
                    )

        TriggerLoadMidiAndMp3 isInitialLoad midiFile mp3File ->
            ( model
            , File.fetchArrayBuffer ("./contents/music/" ++ mp3File)
                |> Task.andThen WebAudioApi.decodeAudioData
                |> Task.andThen
                    (\mp3AudioBuf ->
                        File.fetchArrayBuffer ("./contents/music/" ++ midiFile)
                            |> Task.map ((,) mp3AudioBuf)
                    )
                |> Task.attempt (LoadedMidiAndMp3 isInitialLoad midiFile mp3File)
            )

        LoadedMidiAndMp3 isInitialLoad midiFile _ (Ok ( mp3AudioBuffer, smfBuffer )) ->
            case Byte.decode SmfDecoder.smf smfBuffer of
                Ok smf ->
                    let
                        midi =
                            Midi.fromSmf smf
                    in
                        ( { model
                            | midiContents =
                                model.midiContents
                                    |> Dict.update midiFile
                                        (Maybe.map
                                            (\midiContent ->
                                                { midiContent
                                                    | midiAndMp3 = Just ( midi, mp3AudioBuffer )
                                                }
                                            )
                                        )
                          }
                        , Cmd.none
                        )
                            |> andThen
                                (if isInitialLoad then
                                    flip (,) Cmd.none
                                 else
                                    update (TriggerStart midi mp3AudioBuffer)
                                )

                Err e ->
                    ( { model
                        | error = DecodeError smfBuffer e
                      }
                    , Cmd.none
                    )

        LoadedMidiAndMp3 _ mp3File midiFile (Err s) ->
            Debug.crash <|
                "failed to load file '"
                    ++ midiFile
                    ++ " or file "
                    ++ mp3File
                    ++ "': "
                    ++ s

        Back ->
            ( { model
                | startTime = 0
                , currentTime = 0
                , playing = False
              }
            , Cmd.none
            )

        TriggerStart midi mp3AudioBuffer ->
            ( model
            , Time.now
                |> Task.perform (Start midi mp3AudioBuffer)
            )

        Start midi mp3AudioBuffer currentTime ->
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
                    , futureNotes = prepareFutureNotes (currentTime - startTime) midi
                  }
                , WebAudioApi.play mp3AudioBuffer (currentTime - startTime)
                )
                    |> andThen (update (Tick currentTime))

        Stop ->
            ( { model | playing = False }
            , WebAudioApi.stop
            )

        Fullscreen fullscreen ->
            ( { model | fullscreen = fullscreen }, Cmd.none )

        Close ->
            model.selected
                |> Maybe.map (\_ -> update Stop model)
                |> Maybe.withDefault ( model, Cmd.none )
                |> andThen
                    (\model ->
                        ( { model | selected = Nothing }
                        , Cmd.none
                        )
                    )

        Tick currentTime ->
            ( { model
                | currentTime = currentTime
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
        |> List.concatMap (\( index, track ) -> List.map (Midi.addDetails index track.channel) track.notes)
        |> List.sortBy .position
        |> dropWhile (\note -> Midi.positionToTime midi.timeBase midi.tempo note.position < time)


dropWhile : (a -> Bool) -> List a -> List a
dropWhile f list =
    case list of
        [] ->
            []

        x :: xs ->
            if f x then
                dropWhile f xs
            else
                list


splitWhile : (a -> Bool) -> List a -> List a -> ( List a, List a )
splitWhile f taken list =
    case list of
        [] ->
            ( taken, [] )

        x :: xs ->
            if f x then
                splitWhile f (x :: taken) xs
            else
                ( taken, list )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.playing then
            Time.every (1000 * Time.millisecond / 30) Tick
          else
            Sub.none
        ]


contents : List Content
contents =
    -- 2017
    [ Content "monday-morning" "Monday Morning" "" Nothing (MidiAndMp3 "2017/monday-morning.mid" "2017/monday-morning.mp3" 1250)
    , Content "train-journey" "列車の旅" "「オレ宿」の裏企画「オレが考えた発車ベルその壱」参加曲" (Just "train.jpg") (SoundCloud "318647824")
    , Content
        "good-night"
        "おやすみ"
        """<a href="http://carrotwine.muse.bindsite.jp/myinn.html" target="_blank">「オレが考えた宿で一泊」</a>参加曲"""
        (Just "inn.jpg")
        (SoundCloud "318080873")
    , Content
        "little-world"
        "Little World"
        """<a href="http://carrotwine.muse.bindsite.jp/dtmmeeting4.html" target="_blank">「オレが考えたフィールド曲」</a>参加曲"""
        (Just "field.jpg")
        (SoundCloud "306090165")
      -- 2016
    , Content "hokora" "ほこら" "[「オレが考えたほこらの曲」](http://carrotwine.muse.bindsite.jp/dtmermeeting.html)参加曲" (Just "hokora.jpg") (MidiAndMp3 "2016/hokora.mid" "2016/hokora.mp3" 2770)
    , Content "hokora-fc" "ほこら (FCアレンジ)" "by [ハイデン](https://twitter.com/hydden0310)さん" (Just "hokora.jpg") (Mp3 "2016/hokora-fc.mp3")
    , Content "kira-kira" "Kira Kira" "" Nothing (SoundCloud "278194362")
    , Content "candy" "Candy" "" Nothing (SoundCloud "240810123")
      -- block
    , Content "ancient" "ancient" "スマホゲーム [Block Brothers](http://blockbros.net/) BGM" (Just "block.png") (Mp3 "2016/ancient.mp3")
      -- , Content "beach" "beach" (Just "block.png") (Mp3 "2016/beach.mp3")
    , Content "cloud" "cloud" "スマホゲーム [Block Brothers](http://blockbros.net/) BGM" (Just "block.png") (Mp3 "2016/cloud.mp3")
    , Content "ice" "ice" "スマホゲーム [Block Brothers](http://blockbros.net/) 未収録曲" (Just "block.png") (Mp3 "2016/ice.mp3")
    , Content "jungle" "jungle" "スマホゲーム [Block Brothers](http://blockbros.net/) 未収録曲" (Just "block.png") (Mp3 "2016/jungle.mp3")
      -- , Content "kingdom" "kingdom" Nothing (Mp3 "2016/kingdom.mp3")
      -- , Content "night" "night" Nothing (Mp3 "2016/night.mp3")
    , Content "ninja" "ninja" "スマホゲーム [Block Brothers](http://blockbros.net/) BGM" (Just "block.png") (Mp3 "2016/ninja.mp3")
    , Content "volcano" "volcano" "スマホゲーム [Block Brothers](http://blockbros.net/) 未収録曲" (Just "block.png") (Mp3 "2016/volcano.mp3")
      -- 2015
    , Content "megalopolis" "Megalopolis" "" Nothing (SoundCloud "236197155")
    , Content "voice-of-water" "Voice of Water" "" Nothing (SoundCloud "233781385")
    , Content "wedding-march" "Wedding March" "自作自演" Nothing (SoundCloud "228037751")
    , Content "glass-city" "Glass City" "" Nothing (SoundCloud "200427994")
      -- 2014
    , Content "summer" "Summer" "" Nothing (MidiAndMp3 "2014/summer.mid" "2014/summer.mp3" 1420)
    , Content "sakura" "桜舞う" "" Nothing (MidiAndMp3 "2014/sakura.mid" "2014/sakura.mp3" 1600)
    , Content "midnight" "真夜中の暇つぶし" "" Nothing (MidiAndMp3 "2014/midnight.mid" "2014/midnight.mp3" 540)
      -- 2013
    , Content "string" "糸" "" Nothing (MidiAndMp3 "2013/string.mid" "2013/string.mp3" 840)
    , Content "autumn" "秋風" "" Nothing (MidiAndMp3 "2013/autumn.mid" "2013/autumn.mp3" 1100)
    , Content "afternoon-caos" "午後のカオス" "" Nothing (MidiAndMp3 "2013/afternoon_caos.mid" "2013/afternoon_caos.mp3" 700)
    , Content "michikusa" "道草" "" Nothing (MidiAndMp3 "2013/michikusa.mid" "2013/michikusa.mp3" 860)
    , Content "tmp" "Temporary" "" Nothing (MidiAndMp3 "2013/tmp.mid" "2013/tmp.mp3" 1700)
    , Content "hallucination" "幻覚" "" Nothing (MidiAndMp3 "2013/hallucination.mid" "2013/hallucination.mp3" 1200)
    , Content "blue" "Blue" "" Nothing (MidiAndMp3 "2013/blue.mid" "2013/blue.mp3" 1660)
      -- 2012
    , Content "painter" "変人" "" Nothing (MidiAndMp3 "2012/painter.mid" "2012/painter.mp3" 1800)
    , Content "uploar" "大騒ぎ" "" Nothing (MidiAndMp3 "2012/uploar.mid" "2012/uploar.mp3" 0)
      -- , Content "air" "air" ""　Nothing (MidiAndMp3 "2012/air.mid" "2012/air.mp3" 0) -- TODO: call stack exceeded?
    ]


initialMidiCountents : Dict String MidiContent
initialMidiCountents =
    contents
        |> List.filterMap
            (\content ->
                case content.details of
                    MidiAndMp3 midiFile mp3File delay ->
                        Just ( midiFile, MidiContent midiFile mp3File delay Nothing )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


type alias Content =
    { hash : String
    , title : String
    , description : String
    , image : Maybe String
    , details : Details
    }


type Details
    = Mp3 FileName
    | MidiAndMp3 FileName FileName Time
    | SoundCloud String


type alias MidiContent =
    { midiFile : String
    , mp3File : String
    , delay : Time
    , midiAndMp3 : Maybe ( Midi, AudioBuffer )
    }


view : Model -> Html Msg
view model =
    div
        []
        [ viewHeader
        , main_ [ class "body container" ]
            [ introduction "ジンジャー と Yosuke Torii のホームページ"
            , h2 [] [ Shape.note, text "Music" ]
            , introduction "世界を創る音楽"
            , ul [ class "music-items" ] (List.map (lazy2 viewMusicItem model.selected) contents)
            , case model.error of
                NoError ->
                    text ""

                DecodeError buf e ->
                    text (toString e)
            , h2 [] [ Shape.note, text "Development" ]
            , introduction "プログラミングは芸術"
            , div [ class "repository" ] (GitHub.view model.gitHub |> Tuple.second)
            , h2 [] [ Shape.note, text "Paintings" ]
            , introduction "ペイントでお絵かき"
            , viewPaintings
            , h2 [] [ Shape.note, text "Links" ]
            , introduction ""
            , viewLink
            , viewPlayer model
            ]
        ]


introduction : String -> Html msg
introduction s =
    p [ class "introduction" ] [ text s ]


viewHeader : Html msg
viewHeader =
    header
        [ class "header" ]
        [ div
            [ class "container" ]
            [ h1 [] [ text "World Maker" ] ]
        ]


viewPlayer : Model -> Html Msg
viewPlayer model =
    model.selected
        |> Maybe.map (viewPlayerHelp model)
        |> Maybe.withDefault (text "")


viewPlayerHelp : Model -> Content -> Html Msg
viewPlayerHelp model content =
    div
        [ class "player-container"
        , classList [ ( "player-fullscreen", model.fullscreen ) ]
        ]
        [ case content.details of
            Mp3 mp3File ->
                lazy viewMp3Player mp3File

            MidiAndMp3 midiFile mp3File delay ->
                Dict.get midiFile model.midiContents
                    |> Maybe.map
                        (\midiContent ->
                            midiContent.midiAndMp3
                                |> Maybe.map
                                    (\( midi, mp3 ) ->
                                        MidiPlayer.view
                                            { onBack = Back
                                            , onStart = TriggerStart midi mp3
                                            , onStop = Stop
                                            , onFullscreen = Fullscreen True
                                            , onMinimize = Fullscreen False
                                            , onClose = Close
                                            }
                                            content.hash
                                            model.fullscreen
                                            model.playing
                                            (model.currentTime - model.startTime + delay)
                                            midi
                                    )
                                |> Maybe.withDefault (MidiPlayer.viewLoading Close)
                        )
                    |> Maybe.withDefault (text "")

            SoundCloud id ->
                lazy viewSoundCloudPlayer id
        ]


viewMp3Player : String -> Html Msg
viewMp3Player mp3File =
    div [ class "mp3" ]
        [ audio
            [ src ("./contents/music/" ++ mp3File)
            , autoplay True
            , controls True
            ]
            []
        , MidiPlayer.closeButton Close
        ]


viewSoundCloudPlayer : String -> Html Msg
viewSoundCloudPlayer id =
    div [ class "soundcloud" ] [ soundCloud id, MidiPlayer.closeButton Close ]


viewMusicItem : Maybe Content -> Content -> Html Msg
viewMusicItem selectedContent content =
    let
        selected =
            Just content == selectedContent

        clickMsg =
            if selected then
                Close
            else
                OpenPlayer False content
    in
        case content.details of
            Mp3 mp3File ->
                viewMusicItemHelp clickMsg "music-item-mp3" content.hash content.title content.description selected <|
                    lazy3 viewMusicIcon content.image content.title "MP3"

            MidiAndMp3 midiFile mp3File delay ->
                viewMusicItemHelp clickMsg "music-item-midi-and-mp3" content.hash content.title content.description selected <|
                    lazy3 viewMusicIcon content.image content.title "Midi + MP3"

            SoundCloud id ->
                viewMusicItemHelp clickMsg "music-item-soundcloud" content.hash content.title content.description selected <|
                    lazy3 viewMusicIcon content.image content.title "SoundCloud"


viewMusicIcon : Maybe String -> String -> String -> Html msg
viewMusicIcon maybeUrl alt_ tipe =
    maybeUrl
        |> Maybe.map (\url -> img [ class "music-item-image", src ("./contents/music/jacket/" ++ url), alt alt_ ] [])
        |> Maybe.withDefault (div [ class "music-item-image" ] [ text tipe ])


viewMusicItemHelp : Msg -> String -> String -> String -> String -> Bool -> Html Msg -> Html Msg
viewMusicItemHelp clickMsg class_ id_ label description selected image =
    li
        [ id id_
        , classList
            [ ( "music-item", True )
            , ( "music-item-selected", selected )
            , ( class_, True )
            ]
        , onClick clickMsg
        ]
        [ image
        , div [ class "music-item-label" ] [ text label ]
        , Markdown.toHtml
            [ class "music-item-description"
            , onWithOptions "click" { defaultOptions | stopPropagation = True } (Decode.succeed NoOp)
            ]
            description
        ]


soundCloud : String -> Html msg
soundCloud id =
    iframe
        [ attribute "width" "100%"
        , attribute "height" "20"
        , attribute "scrolling" "no"
        , attribute "frameborder" "no"
        , src <|
            "https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/"
                ++ id
                ++ "&amp;color=ff5500&amp;inverse=false&amp;auto_play=true&amp;show_user=true"
        ]
        []


viewPaintings : Html msg
viewPaintings =
    div
        []
        [ div [ class "paintings-container paintings-container-single" ]
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
        ]


viewLink : Html msg
viewLink =
    ul []
        [ li [] [ a [ href "https://soundcloud.com/jinjor" ] [ text "SoundCloud" ] ]
        , li [] [ a [ href "https://github.com/jinjor" ] [ text "GitHub" ] ]
        , li [] [ a [ href "https://twitter.com/jinjor" ] [ text "Twitter" ] ]
        , li [] [ a [ href "http://jinjor-labo.hatenablog.com/" ] [ text "Blog" ] ]
        ]
