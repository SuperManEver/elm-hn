-- https://hacker-news.firebaseio.com/v0/newstories.json
-- https://hacker-news.firebaseio.com/v0/item/12714713.json?print=pretty

import Html exposing (..)
import Html.App exposing (program)
import Http
import List
import Task
import Json.Decode exposing (Decoder, decodeString, list, int)

main =
  program 
    { init = init 
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model = 
  { allIds : List Int 
  }

-- Init 
init : (Model, Cmd Msg)
init = 
  (Model [], getAllids)

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none

type Msg 
  = LoadIds 
  | FetchFail Http.Error
  | FetchSucceed (List Int)

-- Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    LoadIds -> 
      (model, getAllids)

    FetchSucceed allIds -> 
      ({ model | allIds = allIds }, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)

-- View
view : Model -> Html Msg
view model = 
  let 
    allIds = List.map (\id -> li [] [ text (toString id) ]) model.allIds
  in 
    div [] 
    [
      ul [] allIds
    ]

-- Helper Functions
decodeIds : Decoder (List Int)
decodeIds = list int

getAllids : Cmd Msg 
getAllids = 
  let 
    url = "https://hacker-news.firebaseio.com/v0/newstories.json"
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeIds url)

