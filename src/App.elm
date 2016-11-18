import Html exposing (..)
import Html.App as App exposing (program)
import Html.Events exposing (onClick)
import String exposing (concat)
import Http exposing (Error)
import Task exposing (Task, perform)
import Json.Decode as Json exposing ((:=))
import StoryItem
import Window exposing (height)
import Basics.Extra exposing (never)

latestURL : String 
latestURL = "https://hacker-news.firebaseio.com/v0/newstories.json"

shift : Int 
shift = 40

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
  | LoadMoreStories
  | GetWindowHeight
  | WindowHeight Int


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
        current = List.take shift ids
        newStories = 
          List.map (\ id -> StoryItem.createStory id) current
      in
        {model 
        | storiesIds = (List.drop shift ids)
        , stories = newStories 
        } 
        ! [ Cmd.map StoryMsg (StoryItem.loadStories current) ]

    StoryMsg subMsg -> 
      let 
        (updatedItems, storyItemCmd) = StoryItem.update subMsg model.stories
      in
        {model | stories = updatedItems} ! [ Cmd.map StoryMsg storyItemCmd ]

    LoadMoreStories -> 
      let 
        current = List.take shift model.storiesIds
        newStories = 
          List.map (\ id -> StoryItem.createStory id) current
      in
        { model 
        | storiesIds = (List.drop shift model.storiesIds)
        , stories = model.stories ++ newStories
        } 
        ! [ Cmd.map StoryMsg (StoryItem.loadStories current)]

    WindowHeight height -> 
      Debug.log (toString height)
      model ! []

    GetWindowHeight -> 
      model ! [perform never WindowHeight Window.height]

-- VIEW 
view : Model -> Html Msg
view model = 
  let 
    stories = 
      App.map StoryMsg (StoryItem.view model.stories)
  in
    div [] 
      [ button [ onClick LoadMoreStories ] [ text "Load More" ]
      , button [ onClick GetWindowHeight ] [ text "Height" ]
      , stories
      ]
