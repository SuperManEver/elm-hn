import Html exposing (..)
import Html.App as App exposing (program)
import String exposing (concat)
import Http exposing (Error)
import Task exposing (Task, perform)
import Json.Decode as Json exposing ((:=))

itemUrl : String 
itemUrl = "https://hacker-news.firebaseio.com/v0/item/"

latestStories : String 
latestStories = "https://hacker-news.firebaseio.com/v0/newstories.json"


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
  , stories : List Story
  }

type alias Story = 
  { id : Int 
  , title : String 
  , url : String
  }

createStory : Int -> Story
createStory id = 
  { id = id
  , title = ""
  , url = ""
  }

defaultModel : Story 
defaultModel = 
  { id = 1 
  , title = ""
  , url = ""
  }

type Msg 
  = NoOp
  | StoryFailed Int Http.Error
  | StoryLoaded Int Story
  | LatestFailed Http.Error
  | LatestLoaded (List Int)


-- COMMANDS
idsDecoder : Json.Decoder (List Int)
idsDecoder = 
  Json.list Json.int


loadLatests : Cmd Msg
loadLatests = 
  Task.perform LatestFailed LatestLoaded (Http.get idsDecoder latestStories) 


decoder : Json.Decoder Story
decoder = 
  Json.object3 Story
    ("id" := Json.int)
    ("title" := Json.string)
    ("url" := Json.string)


item : Int -> Task Error Story
item id = 
  Http.get decoder <| concat [ itemUrl, toString id, ".json" ]


loadStory : Int -> Cmd Msg 
loadStory id = 
  let 
    storyLoaded id model = StoryLoaded id model
    storyFailed id error = StoryFailed id error
  in
    perform (storyFailed id) (storyLoaded id) <| item id


loadStories : List Int -> Cmd Msg
loadStories ids = 
  ids
    |> List.map (\ id -> loadStory id) 
    |> Cmd.batch 


-- UPDATE 
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  let 
    updateModel diff id model = 
      List.map (\ val -> if val.id == id then diff val else val) model
  in
    case msg of 
      NoOp -> 
        model ! []

      LatestFailed error -> 
        model ! []

      LatestLoaded ids ->
        let 
          newStories = 
            List.map (\ id -> createStory id) (List.take 20 ids)
        in
          {model 
          | storiesIds = (List.drop 20 ids)
          , stories = newStories 
          } 
          ! [ loadStories (List.take 20 ids) ]

      StoryFailed id _ -> 
        model ! []

      StoryLoaded id item -> 
        let  
          newStories = 
            updateModel 
              (\ val -> item) 
              id 
              model.stories
        in
          {model | stories = newStories} ! []


-- VIEW 
storyView : Story -> Html Msg
storyView {title, url} = 
  div [] 
    [ p [] [ text title ] 
    ]


view : Model -> Html Msg
view model = 
  let 
    stories = List.map (\ story -> storyView story ) model.stories
  in
    div [] stories
