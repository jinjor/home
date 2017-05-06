module Player exposing (..)

import Dict exposing (Dict)
import Task
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import BinaryDecoder.File as File exposing (File)
import BinaryDecoder.Byte as Byte exposing (ArrayBuffer, Error)
import SmfDecoder exposing (Smf)
import Midi exposing (Midi, Note, Detailed)
import MidiPlayer exposing (MidiPlayer)
import Navigation exposing (Location)
import MusicContents exposing (..)
import Core exposing (..)
import WebAudioApi exposing (AudioBuffer)
import RouteParser.QueryString as QueryString


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
    | Close
    | MidiPlayerMsg MidiPlayer.Msg


init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            Model initialMidiCountents Nothing MidiPlayer.init NoError

        firstContent =
            QueryString.parse location.search
                |> Dict.get "content"
                |> Maybe.andThen List.head
                |> Maybe.andThen
                    (\id ->
                        contents
                            |> List.filter (\content -> content.id == id)
                            |> List.head
                    )
    in
        case firstContent of
            Just content ->
                update (OpenPlayer True content) model

            Nothing ->
                ( model, Cmd.none )


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
    viewPlayer model


viewPlayer : Model -> Html Msg
viewPlayer model =
    model.selected
        |> Maybe.map (viewPlayerHelp model)
        |> Maybe.withDefault errorMessage


viewPlayerHelp : Model -> Content -> Html Msg
viewPlayerHelp model content =
    div
        [ class "player-container"
        , classList [ ( "player-fullscreen", True ) ]
        ]
        [ case content.details of
            MidiAndMp3 midiFile mp3File delay ->
                Dict.get midiFile model.midiContents
                    |> Maybe.map
                        (\midiContent ->
                            midiContent.midiAndMp3
                                |> Maybe.map
                                    (\( midi, mp3 ) ->
                                        MidiPlayer.view
                                            { onFullscreen = NoOp
                                            , onMinimize = NoOp
                                            , onClose = Close
                                            , transform = MidiPlayerMsg
                                            }
                                            content.id
                                            content.title
                                            True
                                            midi
                                            mp3
                                            delay
                                            model.midiPlayer
                                    )
                                |> Maybe.withDefault (MidiPlayer.viewLoading Close)
                        )
                    |> Maybe.withDefault errorMessage

            _ ->
                errorMessage
        ]


errorMessage : Html msg
errorMessage =
    div []
        [ div [] [ text "Error: Unable to show player" ]
        , div [] [ a [ href "http://world-maker.com" ] [ text "Go to Home" ] ]
        ]
