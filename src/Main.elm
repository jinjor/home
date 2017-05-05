port module Main exposing (..)

import Dict exposing (Dict)
import Task
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Svg as S
import Svg.Attributes as SA
import BinaryDecoder.File as File exposing (File)
import BinaryDecoder.Byte as Byte exposing (ArrayBuffer, Error)
import SmfDecoder exposing (Smf)
import Midi exposing (Midi, Note, Detailed)
import MidiPlayer exposing (MidiPlayer)
import GitHub exposing (GitHub)
import Markdown
import Navigation exposing (Location)
import Json.Decode as Decode
import UrlParser exposing ((<?>))
import MusicContents exposing (..)
import Core exposing (..)
import WebAudioApi exposing (AudioBuffer)


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
    , midiPlayer : MidiPlayer
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
    | Fullscreen Bool
    | Close
    | GitHubMsg GitHub.Msg
    | MidiPlayerMsg MidiPlayer.Msg


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
                        Model initialMidiCountents Nothing MidiPlayer.init gitHub False NoError

                    firstContent =
                        UrlParser.parsePath parser location
                            |> Maybe.andThen
                                (\maybeId ->
                                    maybeId
                                        |> Maybe.andThen
                                            (\id ->
                                                contents
                                                    |> List.filter (\content -> content.id == id)
                                                    |> List.head
                                            )
                                )
                in
                    case firstContent of
                        Just content ->
                            update (OpenPlayer True content) model
                                |> andThen (\model -> ( model, moveToCard content.id ))

                        Nothing ->
                            ( model, Cmd.none )
            )


parser : UrlParser.Parser (Maybe String -> a) a
parser =
    UrlParser.top <?> UrlParser.stringParam "content"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OpenPlayer isInitialLoad content ->
            model.selected
                |> Maybe.map (\_ -> update (MidiPlayerMsg MidiPlayer.stop) model)
                |> Maybe.withDefault ( model, Cmd.none )
                |> andThen (update <| MidiPlayerMsg MidiPlayer.back)
                |> andThen
                    (if isInitialLoad then
                        update (Fullscreen True)
                     else
                        flip (,) Cmd.none
                    )
                |> andThen
                    (\model ->
                        ( { model | selected = Just content }
                        , Navigation.modifyUrl ("?content=" ++ content.id)
                        )
                    )
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
                                    update (MidiPlayerMsg <| MidiPlayer.triggerStart midi mp3AudioBuffer)
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

        Fullscreen fullscreen ->
            ( { model | fullscreen = fullscreen }, Cmd.none )

        Close ->
            model.selected
                |> Maybe.map (\_ -> update (MidiPlayerMsg MidiPlayer.stop) model)
                |> Maybe.withDefault ( model, Cmd.none )
                |> andThen
                    (\model ->
                        ( { model | selected = Nothing }
                        , Navigation.modifyUrl "/"
                        )
                    )

        GitHubMsg msg ->
            ( { model | gitHub = GitHub.update msg model.gitHub }
            , Cmd.none
            )

        MidiPlayerMsg msg ->
            MidiPlayer.update msg model.midiPlayer
                |> Tuple.mapSecond (Cmd.map MidiPlayerMsg)
                |> andThen
                    (\midiPlayer ->
                        ( { model | midiPlayer = midiPlayer }, Cmd.none )
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map MidiPlayerMsg (MidiPlayer.subscriptions model.midiPlayer)


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
            , headline "Music" "世界を創る音楽"
            , ul [ class "music-items" ] (List.map (lazy2 viewMusicItem model.selected) contents)
            , case model.error of
                NoError ->
                    text ""

                DecodeError buf e ->
                    text (toString e)
            , headline "Development" "プログラミングは芸術"
            , div [ class "repository" ] (GitHub.view model.gitHub |> Tuple.second)
            , headline "Paintings" "ペイントでお絵かき"
            , viewPaintings
            , headline "Links" "主な SNS と連絡先"
            , viewLink
            , viewPlayer model
            ]
        ]


headline : String -> String -> Html msg
headline title comment =
    h2 [] [ note, text title, introduction comment ]


introduction : String -> Html msg
introduction s =
    span [ class "introduction" ] [ text s ]


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
                                            { onFullscreen = Fullscreen True
                                            , onMinimize = Fullscreen False
                                            , onClose = Close
                                            , transform = MidiPlayerMsg
                                            }
                                            content.id
                                            content.title
                                            model.fullscreen
                                            midi
                                            mp3
                                            delay
                                            model.midiPlayer
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
                viewMusicItemHelp clickMsg "music-item-mp3" content.id content.title content.description selected <|
                    lazy3 viewMusicIcon content.image content.title "MP3"

            MidiAndMp3 midiFile mp3File delay ->
                viewMusicItemHelp clickMsg "music-item-midi-and-mp3" content.id content.title content.description selected <|
                    lazy3 viewMusicIcon content.image content.title "Midi + MP3"

            SoundCloud id ->
                viewMusicItemHelp clickMsg "music-item-soundcloud" content.id content.title content.description selected <|
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
    ul [ class "links" ]
        [ li [ class "links-item" ] [ a [ href "https://soundcloud.com/jinjor" ] [ text "SoundCloud(@jinjor)" ] ]
        , li [ class "links-item" ] [ a [ href "https://github.com/jinjor" ] [ text "GitHub(@jinjor)" ] ]
        , li [ class "links-item" ] [ a [ href "https://twitter.com/jinjor" ] [ text "Twitter(@jinjor)" ] ]
        , li [ class "links-item" ] [ a [ href "http://jinjor-labo.hatenablog.com/" ] [ text "ジンジャー研究室（はてなブログ）" ] ]
        , li [ class "links-item" ] [ a [ href "mailto:jinjorweb@gmail.com" ] [ text "メール: jinjorweb@gmail.com" ] ]
        ]


note : Html msg
note =
    S.svg
        [ SA.width "36", SA.height "36", SA.class "shape-note" ]
        [ S.path [ SA.d "M16,6L18,30L20,30L18,8zM18,12L29,15L29,12L16,6zM18,30L15,30z" ] []
        , S.circle [ SA.cx "15", SA.cy "29", SA.r "5" ] []
        ]
