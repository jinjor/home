module Midi exposing
  ( Midi, Track, Note
  , Detailed, addDetails
  , toKey
  , fromSmf
  , positionToTime, timeToPosition
  , toggleVisibility, setMidiOut, setMidiOutToAllTracks
  )

import Time exposing (Time)
import Dict exposing (Dict)
import SmfDecoder as Smf exposing (Smf, MidiEvent(..))


type alias Midi =
  { timeBase : Int
  , tempo : List (Int, Int)
  , tracks : List Track
  }


type alias Track =
  { channel : Int
  , name : String
  , notes : List Note
  , isVisible : Bool
  , portId : Maybe String
  }


type alias Note =
  { position : Int
  , note : Int
  , velocity : Int
  , length : Int
  }


type alias Detailed a =
  { a | track : Int, channel : Int }


addDetails : Int -> Int -> Note -> Detailed Note
addDetails track channel note =
  { position = note.position
  , note = note.note
  , velocity = note.velocity
  , length = note.length
  , channel = channel
  , track = track
  }


toKey : Note -> String
toKey note =
  toString note.position ++ toString note.note


emptyTrack : Track
emptyTrack =
  Track 0 "" [] False Nothing


fromSmf : Smf -> Midi
fromSmf smf =
  let
    tracks =
      smf.tracks
        |> List.tail
        |> Maybe.map (List.map fromSmfTrack)
        |> Maybe.withDefault []

    tempo =
      smf.tracks
        |> List.head
        |> Maybe.map (
             .events
          >> List.foldl updateTrack (0, initContext)
          >> Tuple.second
          >> .tempo
        )
        |> Maybe.withDefault []
  in
    Midi smf.header.timeBase tempo tracks


fromSmfTrack : Smf.Track -> Track
fromSmfTrack track =
  track.events
    |> List.foldl updateTrack (0, initContext)
    |> Tuple.second
    |> (\context ->
      Track context.channel "" (List.reverse context.notes) True Nothing
    )


updateTrack : (Int, MidiEvent) -> (Int, Context) -> (Int, Context)
updateTrack (dtime, e) (position, context) =
  ( position + dtime
  , case e of
      Tempo tempo ->
        { context
          | tempo = (position + dtime, tempo) :: context.tempo
        }

      NoteOn ch note vel ->
        { context
          | channel = max ch context.channel
          , temporaryNotes =
              context.temporaryNotes
                |> Dict.insert note (position + dtime, vel)
        }

      NoteOff ch note ->
        { context
          | temporaryNotes =
              Dict.remove note context.temporaryNotes
          , notes =
              Dict.get note context.temporaryNotes
                |> Maybe.map (\(startPos, vel) ->
                    Note startPos note vel (position + dtime - startPos)
                      :: context.notes
                  )
                |> Maybe.withDefault context.notes
        }

      _ ->
        context
  )


type alias Context =
  { channel : Int
  , temporaryNotes : Dict Int (Int, Int)
  , notes : List Note
  , tempo : List (Int, Int)
  }


initContext : Context
initContext =
  Context 0 Dict.empty [] []


positionToTime : Int -> List (Int, Int) -> Int -> Time
positionToTime timeBase tempoStack position =
  tempoStack
    |> List.foldl (\(from, tempo) (to, total) ->
      if from > to then
        (to, total)
      else
        (from, total + toFloat (to - from) / toFloat timeBase * toFloat tempo / 1000)
    ) (position, 0.0)
    |> Tuple.second


tempoOfBpm : Int -> Int
tempoOfBpm bpm =
  60 * 1000 * 1000 // bpm


timeToPosition : Int -> List (Int, Int) -> Time -> Int
timeToPosition timeBase tempoStack time =
  timeToPositionHelp timeBase (List.reverse tempoStack) time (0, tempoOfBpm 120)


timeToPositionHelp : Int -> List (Int, Int) -> Time -> (Int, Int) -> Int
timeToPositionHelp timeBase tempo time (from, currentTempo) =
  case tempo of
    [] ->
      from + round (time * 1000 / toFloat currentTempo * toFloat timeBase)

    (to, nextTempo) :: xs ->
      let
        nextTime =
          toFloat (to - from) / toFloat timeBase * toFloat currentTempo / 1000
      in
        if nextTime > time then
          from + round (time * 1000 / toFloat currentTempo * toFloat timeBase)
        else
          timeToPositionHelp timeBase xs (time - nextTime) (to, nextTempo)


toggleVisibility : Int -> Midi -> Midi
toggleVisibility index midi =
  { midi
    | tracks =
        midi.tracks
          |> List.indexedMap (\i track ->
              if i == index then
                { track | isVisible = not track.isVisible }
              else
                track
            )
  }


setMidiOutToAllTracks : String -> Midi -> Midi
setMidiOutToAllTracks portId midi =
  { midi
    | tracks =
        midi.tracks
          |> List.map (\track -> { track | portId = Just portId })
  }


setMidiOut : Int -> String -> Midi -> Midi
setMidiOut index portId midi =
  { midi
    | tracks =
        midi.tracks
          |> List.indexedMap (\i track ->
              if i == index then
                { track | portId = Just portId }
              else
                track
            )
  }
