module MusicContents exposing (..)

import Time exposing (Time)
import Json.Encode exposing (..)


type alias Json =
    Json.Encode.Value


type alias FileName =
    String


type alias Content =
    { id : String
    , title : String
    , description : String
    , image : Maybe String
    , details : Details
    }


type Details
    = Mp3 FileName
    | MidiAndMp3 FileName FileName Time
    | SoundCloud String


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
    , Content
        "hokora"
        "ほこら"
        """<a href="http://carrotwine.muse.bindsite.jp/dtmermeeting.html" target="_blank">「オレが考えたほこらの曲」</a>参加曲"""
        (Just "hokora.jpg")
        (MidiAndMp3 "2016/hokora.mid" "2016/hokora.mp3" 2770)
    , Content
        "hokora-fc"
        "ほこら (FCアレンジ)"
        """by <a href="https://twitter.com/hydden0310" target="_blank">ハイデン</a>さん"""
        (Just "hokora.jpg")
        (Mp3 "2016/hokora-fc.mp3")
    , Content "kira-kira" "Kira Kira" "" Nothing (SoundCloud "278194362")
    , Content "candy" "Candy" "" Nothing (SoundCloud "240810123")
      -- block
    , Content
        "animal-kingdom"
        "Animal Kingdom"
        """スマホゲーム <a href="http://blockbros.net/" target="_blank">Block Brothers</a> BGM"""
        (Just "block.png")
        (Mp3 "2016/animal-kingdom.mp3")
      -- , Content "summer-beach" "Summer Beach" (Just "block.png") (Mp3 "2016/summer-beach.mp3")
    , Content
        "cloud-passage"
        "Cloud Passage"
        """スマホゲーム <a href="http://blockbros.net/" target="_blank">Block Brothers</a> BGM"""
        (Just "block.png")
        (Mp3 "2016/cloud-passage.mp3")
    , Content
        "ice-world"
        "Ice World"
        """スマホゲーム <a href="http://blockbros.net/" target="_blank">Block Brothers</a> 未収録曲"""
        (Just "block.png")
        (Mp3 "2016/ice-world.mp3")
    , Content
        "welcome-to-the-jungle"
        "Welcome to the Jungle!"
        """スマホゲーム <a href="http://blockbros.net/" target="_blank">Block Brothers</a> 未収録曲"""
        (Just "block.png")
        (Mp3 "2016/welcome-to-the-jungle.mp3")
      -- , Content "kingdom" "kingdom" Nothing (Mp3 "2016/kingdom.mp3")
      -- , Content "night" "night" Nothing (Mp3 "2016/night.mp3")
    , Content
        "moon-over-the-castle"
        "Moon Over The Castle"
        """スマホゲーム <a href="http://blockbros.net/" target="_blank">Block Brothers</a> BGM"""
        (Just "block.png")
        (Mp3 "2016/moon-over-the-castle.mp3")
    , Content
        "lava-mountain"
        "Lava Mountain"
        """スマホゲーム <a href="http://blockbros.net/" target="_blank">Block Brothers</a> 未収録曲"""
        (Just "block.png")
        (Mp3 "2016/lava-mountain.mp3")
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


json : Json
json =
    encodeContents contents


encodeContents : List Content -> Json
encodeContents contents =
    list (List.map encodeContent contents)


encodeContent : Content -> Json
encodeContent content =
    object
        [ ( "id", string content.id )
        , ( "title", string content.title )
        , ( "description", string content.description )
        , ( "image", content.image |> Maybe.withDefault "" |> string )
        , ( "details", encodeDetails content.details )
        ]


encodeDetails : Details -> Json
encodeDetails details =
    case details of
        Mp3 fileName ->
            object [ ( "mp3", string fileName ) ]

        MidiAndMp3 midi mp3 delay ->
            object [ ( "midi", string midi ), ( "mp3", string mp3 ), ( "delay", float delay ) ]

        SoundCloud id ->
            object [ ( "id", string id ) ]
