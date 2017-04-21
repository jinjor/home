module MidiPlayer exposing (Options, view, viewLoading, viewClosed)

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


type alias Options msg =
  { onBack : msg
  , onStart : msg
  , onStop : msg
  , onClose : msg
  }


type alias NoteColor =
  { highlight : String
  , normal : String
  }


colors : List NoteColor
colors =
  List.map2 NoteColor (Colors.depth 1) (Colors.depth 5)


view : Options msg -> Bool -> Time -> Midi -> Html msg
view options playing time midi =
  let
    currentPosition =
      Midi.timeToPosition midi.timeBase time
  in
    div
      [ HA.class "midi-player midi-player-selected"
      ]
      [ midi.tracks
          |> List.map2 (viewTrack currentPosition) colors
          |> svg (svgAttributes currentPosition)
      , centerLine
      , lazy3 control options midi.tracks playing
      ]


viewLoading : msg -> Html msg
viewLoading onClose =
  div
    [ HA.class "midi-player midi-player-selected"
    ]
    [ disabledControl onClose
    ]


viewClosed : msg -> Html msg
viewClosed onLoad =
  div
    [ HA.class "midi-player-empty"
    , onClick onLoad
    ]
    [ H.text "Play" ]


centerLine : Html msg
centerLine =
  div
    [ HA.style
        [ "border-right" => "solid 1px #555"
        , "height" => "270px"
        , "left" => "50%"
        , "top" => "0"
        , "position" => "absolute"
        ]
    ]
    []


svgAttributes : Int -> List (S.Attribute msg)
svgAttributes currentPosition =
  [ SA.width "10000"
  , SA.height "90"
  , viewBox (String.join " " <| List.map toString [currentPosition - 5000, 0, 10000, 90])
  , preserveAspectRatio "none"
  , HA.style
      [ "width" => "100%"
      , "height" => "270px"
      , "background-color" => "black"
      , "display" => "block"
      ]
  ]


control : Options msg -> List Track -> Bool -> Html msg
control options tracks playing =
  div
    [ HA.class "midi-player-control" ]
    [ backButton options
    , playButton options playing
    , closeButton options.onClose
    ]


disabledControl : msg -> Html msg
disabledControl onClose =
  div
    [ HA.class "midi-player-control" ]
    [ closeButton onClose ]


backButton : Options msg -> Html msg
backButton options =
  controlButton
    ( onClick options.onBack )
    ( S.path [ SA.fill "#ddd", back ] [] )


playButton : Options msg -> Bool -> Html msg
playButton options playing =
  controlButton
    ( onClick ( if playing then options.onStop else options.onStart ) )
    ( S.path [ SA.fill "#ddd", if playing then stop else start ] [] )


closeButton : msg -> Html msg
closeButton onClose =
  controlButton
    ( onClick onClose )
    ( S.path [ SA.stroke "#ddd", SA.strokeWidth "2", close ] [] )


controlButton : H.Attribute msg -> Svg msg -> Html msg
controlButton event inner =
  div
    [ event, HA.style buttonStyles ]
    [ svg [ SA.width "40", SA.height "30" ] [ inner ] ]


back : S.Attribute msg
back =
  SA.d "M12,10v10h2v-10zm14,0v10l-12,-5z"


start : S.Attribute msg
start =
  SA.d "M10,8v14l16,-7z"


close : S.Attribute msg
close =
  SA.d "M11,9L25,21zM11,21L25,9z"


stop : S.Attribute msg
stop =
  SA.d "M10,8v14h4v-14zm10,0v14h4v-14z"


buttonStyles : List (String, String)
buttonStyles =
  [ "width" => "40px"
  , "bottom" => "0"
  , "text-align" => "center"
  ]


viewTrack : Int -> NoteColor -> Track -> Html msg
viewTrack currentPosition color track =
  if track.isVisible then
    track.notes
      |> List.filterMap (\note ->
        if currentPosition < note.position + note.length + 5000 && currentPosition > note.position - 5000 then
          Just (Midi.toKey note, lazy2 viewNote (noteColor color note currentPosition) note)
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
(=>) = (,)


px : a -> String
px num =
  toString num ++ "px"
