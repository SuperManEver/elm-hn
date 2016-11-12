import Html exposing (..)
import Html.App as App exposing (program)
import Json.Decode exposing (Decoder, list, int)
import Http
import Task
import StoryItem

main = 
  program 
    { init = init 
    , update = update 
    , view = view 
    , subscriptions = \ _ -> Sub.none
    }


-- MODEL 
type alias Model =
  { storyIds : List Int 
  , stories : List StoryItem.Model
  }

type Msg 
  = NoOp
  | LoadAllIds 
  | FetchFail Http.Error
  | FetchSucceed (List Int)
  | StoryItemsMsg StoryItem.Msg


-- INIT
init : (Model, Cmd Msg)
init =  
  Model [] [] ! [ getAllIds ]


-- COMMANDS 
getAllIds : Cmd Msg 
getAllIds =
  let 
    url = "https://hacker-news.firebaseio.com/v0/newstories.json"
    decodeIds = list int
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeIds url)


-- UPDATE 
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp ->
      model ! []

    LoadAllIds -> 
      model ! [ getAllIds ]

    FetchSucceed allIds -> 
      let 
        stories = List.map (\ id -> StoryItem.createStory id) (List.take 10 allIds)
      in
        { model | storyIds = List.drop 10 allIds, stories = stories } ! []

    FetchFail _ -> 
      model ! []

    StoryItemsMsg subMsg -> 
      model ! []


-- VIEW
view : Model -> Html Msg 
view model = 
  let 
    items = App.map StoryItemsMsg (StoryItem.view model.storyIds)
    -- items = List.map (\ id -> p [] [ text <| toString id ]) model.storyIds
  in 
    div [] 
      [ items 
      ]
      


