module GitHub exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http


type alias User =
  { avatarUrl : String -- "https://avatars1.githubusercontent.com/u/2568148?v=3"
  , bio : String -- null
  -- , company : String -- null
  -- , createdAt : String -- "2012-10-15T23:01:43Z"
  -- , email : String -- "jinjorweb@gmail.com"
  , followers : Int
  , following : Int
  , htmlUrl : String -- "https://github.com/jinjor"
  , location : String -- "Japan"
  , name : String -- "Yosuke Torii"
  -- , publicGists : Int
  , publicRepos : Int
  -- , type_ : String -- User
  -- , updatedAt : String -- "2016-12-29T17:05:42Z"
  }


type alias Repository =
  -- { createdAt : String -- "2016-07-17T15:56:06Z"
  -- , defaultBranch : String -- "master"
  -- ,
  { description : String -- "A diff implementation for Elm"
  -- , fork : Bool -- false
  , forks : Int
  -- , forksCount : Int
  , fullName : String -- "jinjor/elm-diff"
  , homepage : String -- "http://package.elm-lang.org/packages/jinjor/elm-diff/latest"
  , language : String -- "Elm"
  , name : String -- "elm-diff"
  -- , networkCount : Int
  -- , openIssues : Int
  -- , openIssuesCount : Int
  -- , owner : User
  -- , private : Bool -- false
  -- , pushedAt : String -- "2016-11-14T07:47:34Z"
  -- , size : Int -- 13
  -- , sshUrl : String -- "git@github.com:jinjor/elm-diff.git"
  , stargazersCount : Int
  -- , subscribersCount : Int
  -- , updateAt : String -- "2017-04-03T01:49:57Z"
  , watchers : Int
  -- , watchersCount : Int
  }


type alias GitHub =
  { user : Maybe (String, Maybe User)
  , repos : List (String, Maybe Repository)
  }


type Msg
  = ReceiveUser (Result Http.Error User)
  | ReceiveRepository (Result Http.Error Repository)


init : (Msg -> msg) -> Maybe String -> List String -> (GitHub, Cmd msg)
init tagger maybeUserName reposNames =
  let
    gitHub =
      GitHub
        ( maybeUserName |> Maybe.map (flip (,) Nothing) )
        ( reposNames |> List.map (flip (,) Nothing) )

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
                |> List.map (\(name, r) ->
                  if repos.fullName == name then
                    (name, Just repos)
                  else
                    (name, r)
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


view : GitHub -> (Html msg, List (Html msg))
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


-- repositoryCards : List (String, Maybe Repository) -> Html msg
-- repositoryCards repositories =
--   repositories
--     |> List.map repositoryCard
--     |> ul []


repositoryCard : (String, Maybe Repository) -> Html msg
repositoryCard (name, maybeRepos) =
  maybeRepos
    |> Maybe.map repositoryCardHelp
    |> Maybe.withDefault ( emptyRepositoryCard name )


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
        ] [ text fullName ]
    ]
