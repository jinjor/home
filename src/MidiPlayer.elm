module MidiPlayer
    exposing
        ( MidiPlayer
        , init
        , Msg
        , back
        , triggerStart
        , stop
        , Options
        , update
        , subscriptions
        , view
        , viewLoading
        , closeButton
        )

import Time exposing (Time)
import Html as H exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Keyed
import Midi exposing (..)
import Colors
import Http
import WebAudioApi exposing (AudioBuffer)
import Midi
import Core exposing (..)
import Task exposing (Task)


type alias MidiPlayer =
    { playing : Bool
    , startTime : Time
    , currentTime : Time
    , futureNotes : List (Detailed Note)
    }


init : MidiPlayer
init =
    MidiPlayer False 0 0 []


type Msg
    = Back
    | TriggerStart Midi AudioBuffer
    | Start Midi AudioBuffer Time
    | Stop
    | Tick Time


back : Msg
back =
    Back


triggerStart : Midi -> AudioBuffer -> Msg
triggerStart =
    TriggerStart


stop : Msg
stop =
    Stop


update : Msg -> MidiPlayer -> ( MidiPlayer, Cmd Msg )
update msg model =
    case msg of
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

        Tick currentTime ->
            ( { model
                | currentTime = currentTime
              }
            , Cmd.none
            )


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


subscriptions : MidiPlayer -> Sub Msg
subscriptions model =
    if model.playing then
        Time.every (1000 * Time.millisecond / 30) Tick
    else
        Sub.none


prepareFutureNotes : Time -> Midi -> List (Detailed Note)
prepareFutureNotes time midi =
    midi.tracks
        |> List.indexedMap (,)
        |> List.concatMap (\( index, track ) -> List.map (Midi.addDetails index track.channel) track.notes)
        |> List.sortBy .position
        |> dropWhile (\note -> Midi.positionToTime midi.timeBase midi.tempo note.position < time)


type alias Options msg =
    { onFullscreen : msg
    , onMinimize : msg
    , onClose : msg
    , transform : Msg -> msg
    }


type alias NoteColor =
    { highlight : String
    , normal : String
    }


colors : List NoteColor
colors =
    List.map2 NoteColor (Colors.depth 1) (Colors.depth 5)


view : Options msg -> String -> String -> Bool -> Midi -> AudioBuffer -> Time -> MidiPlayer -> Html msg
view options id title fullscreen midi mp3AudioBuffer delay model =
    let
        time =
            model.currentTime - model.startTime + delay

        currentPosition =
            Midi.timeToPosition midi.timeBase midi.tempo time
    in
        div
            [ HA.class "midi-player"
            ]
            [ midi.tracks
                |> List.map2 (lazy3 viewTrack currentPosition) colors
                |> svg (svgAttributes currentPosition)
            , centerLine
            , lazy viewTitle title
            , control options id title fullscreen midi.tracks midi mp3AudioBuffer model.playing
            ]


viewTitle : String -> Html msg
viewTitle title =
    H.div [ HA.class "midi-player-title" ] [ H.text title ]


viewLoading : msg -> Html msg
viewLoading onClose =
    div
        [ HA.class "midi-player"
        ]
        [ svg (svgAttributes 0) []
        , div [ HA.class "midi-player-loading" ] [ H.text "Loading" ]
        , disabledControl onClose
        ]


centerLine : Svg msg
centerLine =
    div [ HA.class "midi-player-center-line" ] []


svgAttributes : Int -> List (S.Attribute msg)
svgAttributes currentPosition =
    [ SA.width "10000"
    , SA.height "90"
    , viewBox (String.join " " <| List.map toString [ currentPosition - 5000, 0, 10000, 90 ])
    , preserveAspectRatio "none"
    , SA.class "midi-player-display"
    ]


control : Options msg -> String -> String -> Bool -> List Track -> Midi -> AudioBuffer -> Bool -> Html msg
control options id title fullscreen tracks midi mp3AudioBuffer playing =
    div
        [ HA.class "midi-player-control" ]
        [ backButton
            |> H.map options.transform
        , lazy3 playButton midi mp3AudioBuffer playing
            |> H.map options.transform
        , if fullscreen then
            lazy miniButton options
          else
            lazy fullButton options
        , lazy3 tweetButton options id title
        , lazy closeButton options.onClose
        ]


disabledControl : msg -> Html msg
disabledControl onClose =
    div
        [ HA.class "midi-player-control" ]
        [ div [ HA.class "midi-player-control-spacer" ] []
        , closeButton onClose
        ]


backButton : Html Msg
backButton =
    controlButton
        [ onClick Back ]
        (S.path [ SA.fill "#ddd", SA.d "M12,10v10h2v-10zm14,0v10l-12,-5z" ] [])


playButton : Midi -> AudioBuffer -> Bool -> Html Msg
playButton midi mp3AudioBuffer playing =
    controlButton
        [ onClick
            (if playing then
                Stop
             else
                TriggerStart midi mp3AudioBuffer
            )
        ]
        (S.path
            [ SA.fill "#ddd"
            , if playing then
                SA.d "M10,8v14h4v-14zm10,0v14h4v-14z"
              else
                SA.d "M10,8v14l16,-7z"
            ]
            []
        )


fullButton : Options msg -> Html msg
fullButton options =
    controlButton
        [ onClick options.onFullscreen ]
        (S.path [ SA.stroke "#ddd", SA.strokeWidth "2", fill "transparent", SA.d "M11,9V21H25V9z" ] [])


miniButton : Options msg -> Html msg
miniButton options =
    controlButton
        [ onClick options.onMinimize ]
        (S.path [ SA.stroke "#ddd", SA.strokeWidth "2", fill "transparent", SA.d "M13,21H23" ] [])


tweetButton : Options msg -> String -> String -> Html msg
tweetButton options id title =
    controlButton
        []
        (S.a
            [ SA.xlinkHref (tweetUrl id title)
            , SA.target "_blank"
            ]
            [ S.image
                [ SA.width "30"
                , SA.height "30"
                , SA.xlinkHref "./assets/Twitter_Logo_White_On_Image.svg"
                ]
                []
            ]
        )


tweetUrl : String -> String -> String
tweetUrl id title =
    "https://twitter.com/intent/tweet"
        ++ ("?original_referer=" ++ Http.encodeUri "http://world-maker.com")
        ++ "&ref_src=twsrc%5Etfw"
        ++ ("&text=♪" ++ Http.encodeUri (title ++ " - ジンジャー"))
        ++ "&tw_p=tweetbutton"
        ++ ("&url=http://world-maker.com/?content=" ++ Http.encodeUri id)


closeButton : msg -> Html msg
closeButton onClose =
    controlButton
        [ onClick onClose ]
        (S.path [ SA.stroke "#ddd", SA.strokeWidth "2", SA.d "M11,9L25,21zM11,21L25,9z" ] [])


controlButton : List (H.Attribute msg) -> Svg msg -> Html msg
controlButton attributes inner =
    div
        (SA.class "midi-player-control-button" :: attributes)
        [ svg [ SA.width "40", SA.height "30" ] [ inner ] ]


viewTrack : Int -> NoteColor -> Track -> Html msg
viewTrack currentPosition color track =
    if track.isVisible then
        track.notes
            |> List.filterMap
                (\note ->
                    if currentPosition < note.position + note.length + 5000 && currentPosition > note.position - 5000 then
                        Just ( Midi.toKey note, lazy2 viewNote (noteColor color note currentPosition) note )
                    else
                        Nothing
                )
            |> Svg.Keyed.node "g" []
    else
        Svg.Keyed.node "g" [] []


noteColor : NoteColor -> Note -> Int -> String
noteColor color note currentPosition =
    if note.position < currentPosition && currentPosition < note.position + note.length then
        color.highlight
    else
        color.normal


viewNote : String -> Note -> Html msg
viewNote color note =
    rect
        [ x (toString <| note.position)
        , y (toString <| 90 - (note.note - 60 + 45))
        , SA.width (toString note.length)
        , SA.height "1"
        , fill color
        ]
        []


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


px : a -> String
px num =
    toString num ++ "px"
