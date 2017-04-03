module GitHub exposing (..)


import Json.Decode as Decode exposing (Decoder)
import Html exposing (..)
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
  -- , fullName : String -- "jinjor/elm-diff"
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


getUser : (Result Http.Error User -> msg) -> String -> Cmd msg
getUser tagger name =
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


getRepository : (Result Http.Error Repository -> msg) -> String -> String -> Cmd msg
getRepository tagger userName reposName =
  Http.get ("https://api.github.com/repos/" ++ userName ++ "/" ++ reposName) decodeRepository
    |> Http.send tagger


decodeRepository : Decoder Repository
decodeRepository =
  Decode.map7 Repository
    (Decode.field "description" Decode.string)
    (Decode.field "forks" Decode.int)
    (Decode.field "homepage" Decode.string)
    (Decode.field "language" Decode.string)
    (Decode.field "name" Decode.string)
    (Decode.field "stargazers_count" Decode.int)
    (Decode.field "watchers" Decode.int)


-- getLanguages
-- "https://api.github.com/repos/jinjor/elm-diff/languages"


userCard : User -> Html msg
userCard user =
  text ""


repositoryCard : Repository -> Html msg
repositoryCard repos =
  text ""
