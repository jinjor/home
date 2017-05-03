module GitHub exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http


type alias User =
    { avatarUrl : String
    , bio : String
    , followers : Int
    , following : Int
    , htmlUrl : String
    , location : String
    , name : String
    , publicRepos : Int
    }


type alias Repository =
    { description : String
    , forks : Int
    , fullName : String
    , homepage : String
    , language : String
    , name : String
    , stargazersCount : Int
    , watchers : Int
    }


type alias GitHub =
    { user : Maybe ( String, Maybe User )
    , repos : List ( String, Maybe Repository )
    }


type Msg
    = ReceiveUser (Result Http.Error User)
    | ReceiveRepository (Result Http.Error Repository)


init : (Msg -> msg) -> Maybe String -> List String -> ( GitHub, Cmd msg )
init tagger maybeUserName reposNames =
    let
        gitHub =
            GitHub
                (maybeUserName |> Maybe.map (flip (,) Nothing))
                (reposNames |> List.map (flip (,) Nothing))

        u =
            maybeUserName
                |> Maybe.map (fetchUser (ReceiveUser >> tagger))
                |> Maybe.withDefault Cmd.none

        r =
            reposNames
                |> List.map (fetchRepository (ReceiveRepository >> tagger))
    in
        ( gitHub, Cmd.batch (u :: r) )


update : Msg -> GitHub -> GitHub
update msg gitHub =
    case msg of
        ReceiveUser (Ok user) ->
            { gitHub
                | user =
                    gitHub.user
                        |> Maybe.map (Tuple.mapSecond (always (Just user)))
            }

        ReceiveRepository (Ok repos) ->
            { gitHub
                | repos =
                    gitHub.repos
                        |> List.map
                            (\( name, r ) ->
                                if repos.fullName == name then
                                    ( name, Just repos )
                                else
                                    ( name, r )
                            )
            }

        _ ->
            gitHub


fetchUser : (Result Http.Error User -> msg) -> String -> Cmd msg
fetchUser tagger name =
    Http.get ("https://api.github.com/users/" ++ name) decodeUser
        |> Http.send tagger


decodeUser : Decoder User
decodeUser =
    Decode.map8 User
        (Decode.field "avatar_url" Decode.string)
        (Decode.field "bio" Decode.string)
        (Decode.field "followers" Decode.int)
        (Decode.field "following" Decode.int)
        (Decode.field "html_url" Decode.string)
        (Decode.field "location" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "public_repos" Decode.int)


fetchRepository : (Result Http.Error Repository -> msg) -> String -> Cmd msg
fetchRepository tagger fullName =
    Http.get ("https://api.github.com/repos/" ++ fullName) decodeRepository
        |> Http.send tagger


decodeRepository : Decoder Repository
decodeRepository =
    Decode.map8 Repository
        (Decode.field "description" Decode.string)
        (Decode.field "forks" Decode.int)
        (Decode.field "full_name" Decode.string)
        (Decode.field "homepage" Decode.string)
        (Decode.field "language" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "stargazers_count" Decode.int)
        (Decode.field "watchers" Decode.int)



-- getLanguages
-- "https://api.github.com/repos/jinjor/elm-diff/languages"


view : GitHub -> ( Html msg, List (Html msg) )
view gitHub =
    ( gitHub.user
        |> Maybe.andThen (Tuple.second)
        |> Maybe.map userCard
        |> Maybe.withDefault (text "")
    , gitHub.repos
        |> List.map repositoryCard
    )


userCard : User -> Html msg
userCard user =
    text ""


repositoryCard : ( String, Maybe Repository ) -> Html msg
repositoryCard ( name, maybeRepos ) =
    maybeRepos
        |> Maybe.map repositoryCardHelp
        |> Maybe.withDefault (emptyRepositoryCard name)


repositoryCardHelp : Repository -> Html msg
repositoryCardHelp repos =
    li [ class "repository-card" ]
        [ a
            [ class "repository-card-name"
            , href ("https://github.com/" ++ repos.fullName)
            ]
            [ text repos.name ]
        , div [ class "repository-card-description" ] [ text repos.description ]
        , div [ class "repository-card-stats" ]
            [ div
                [ class "repository-card-stats-language" ]
                [ span [ class "repository-card-stats-language-color" ] []
                , text repos.language
                ]
            , a
                [ class "repository-card-stats-stargazers"
                , href ("https://github.com/" ++ repos.fullName ++ "/stargazers")
                , target "_blank"
                ]
                [ text (toString repos.stargazersCount)
                , text " stars"
                ]
            , a
                [ class "repository-card-stats-forks"
                , href ("https://github.com/" ++ repos.fullName ++ "/network")
                , target "_blank"
                ]
                [ text (toString repos.forks)
                , text " forks"
                ]
            ]
        ]


emptyRepositoryCard : String -> Html msg
emptyRepositoryCard fullName =
    li [ class "repository-card" ]
        [ a
            [ class "repository-card-name"
            , href ("https://github.com/" ++ fullName)
            ]
            [ text fullName ]
        ]
