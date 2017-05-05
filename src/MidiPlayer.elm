module MidiPlayer exposing (Options, view, viewLoading, closeButton)

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


type alias Options msg =
    { onBack : msg
    , onStart : msg
    , onStop : msg
    , onFullscreen : msg
    , onMinimize : msg
    , onClose : msg
    }


type alias NoteColor =
    { highlight : String
    , normal : String
    }


colors : List NoteColor
colors =
    List.map2 NoteColor (Colors.depth 1) (Colors.depth 5)


view : Options msg -> String -> String -> Bool -> Bool -> Time -> Midi -> Html msg
view options id title fullscreen playing time midi =
    let
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
            , control options id title fullscreen midi.tracks playing
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


control : Options msg -> String -> String -> Bool -> List Track -> Bool -> Html msg
control options id title fullscreen tracks playing =
    div
        [ HA.class "midi-player-control" ]
        [ lazy backButton options
        , lazy2 playButton options playing
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


backButton : Options msg -> Html msg
backButton options =
    controlButton
        [ onClick options.onBack ]
        (S.path [ SA.fill "#ddd", SA.d "M12,10v10h2v-10zm14,0v10l-12,-5z" ] [])


playButton : Options msg -> Bool -> Html msg
playButton options playing =
    controlButton
        [ onClick
            (if playing then
                options.onStop
             else
                options.onStart
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
