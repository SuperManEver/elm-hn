module StoryItem exposing (..)

import Html exposing (..)
import Task exposing (Task, perform)
import Json.Decode as Json exposing ((:=))
import Http exposing (Error)
import String exposing (concat)

itemUrl : String 
itemUrl = "https://hacker-news.firebaseio.com/v0/item/"

-- MODEL 
type alias Model = 
  { id : Int 
  , title : String 
  , url : String
  }

createStory : Int -> Model
createStory id = 
  { id = id
  , title = ""
  , url = ""
  }

defaultModel : Model 
defaultModel = 
  { id = 1 
  , title = ""
  , url = ""
  }


-- COMMANDS
decoder : Json.Decoder Model
decoder = 
  Json.object3 Model
    ("id" := Json.int)
    ("title" := Json.string)
    ("url" := Json.string)


item : Int -> Task Error Model
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
type Msg 
  = NoOp
  | StoryFailed Int Http.Error
  | StoryLoaded Int Model


update : Msg -> List Model -> (List Model, Cmd Msg)
update msg model = 
  let 
    updateModel diff id model = 
      List.map (\ val -> if val.id == id then diff val else val) model
  in
    case msg of 
      NoOp -> 
        model ! []

      StoryFailed id _ -> 
          model ! []

      StoryLoaded id item -> 
        let  
          stories = updateModel (\ val -> item) id model
        in
          stories ! []


-- VIEW 
viewItem : Model -> Html Msg
viewItem {title, url} = 
  div [] 
    [ p [] [ text title ] 
    ]

view : List Model -> Html Msg 
view stories = 
  let 
    xs = List.map (\ story -> viewItem story) stories
  in
    div [] xs
