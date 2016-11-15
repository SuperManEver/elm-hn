import Html exposing (..)
import Html.App as App exposing (program)
import String exposing (concat)
import Http exposing (Error)
import Task exposing (Task, perform)
import Json.Decode as Json exposing ((:=))
import StoryItem

latestURL : String 
latestURL = "https://hacker-news.firebaseio.com/v0/newstories.json"


-- MAIN 
main = program
  { init = init 
  , update = update 
  , view = view 
  , subscriptions = \ _ -> Sub.none
  }


-- INIT  
init : (Model, Cmd Msg)
init = 
  Model [] [] ! [ loadLatests ]


-- MODEL 
type alias Model = 
  { storiesIds : List Int
  , stories : List StoryItem.Model
  }

type Msg 
  = NoOp
  | LatestFailed Http.Error
  | LatestLoaded (List Int)
  | StoryMsg StoryItem.Msg 


-- COMMANDS
idsDecoder : Json.Decoder (List Int)
idsDecoder = 
  Json.list Json.int


loadLatests : Cmd Msg
loadLatests = 
  Task.perform LatestFailed LatestLoaded (Http.get idsDecoder latestURL) 


-- UPDATE 
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    LatestFailed error -> 
      model ! []

    LatestLoaded ids ->
      let 
        newStories = 
          List.map (\ id -> StoryItem.createStory id) (List.take 20 ids)
      in
        {model 
        | storiesIds = (List.drop 20 ids)
        , stories = newStories 
        } 
        ! [ Cmd.map StoryMsg (StoryItem.loadStories newStories) ]

    StoryMsg subMsg -> 
      let 
        (updatedItems, storyItemCmd) = StoryItem.update subMsg model.stories
      in
        {model | stories = updatedItems} ! [ Cmd.map StoryMsg storyItemCmd ]


-- VIEW 
view : Model -> Html Msg
view model = 
  let 
    stories = List.map (\ story -> App.map StoryMsg (StoryItem.view story) ) model.stories
  in
    div [] stories
