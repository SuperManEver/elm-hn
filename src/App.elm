import Html exposing (..)
import Html.App as App exposing (program)
import String exposing (concat)
import Http exposing (Error)
import Task exposing (Task, perform)
import Json.Decode as Json exposing ((:=))

itemUrl : String 
itemUrl = "https://hacker-news.firebaseio.com/v0/item/"

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
  Model [] ! [ loadStories [12941832,12941824,12941820,12941790,12941757,12941726] ]


-- MODEL 
type alias Model = 
  { stories : List Story
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


-- COMMANDS
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
    storyLoaded : Int -> Story -> Msg
    storyLoaded id model = 
      StoryLoaded id model

    storyFailed id error = 
      StoryFailed id error

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
  case msg of 
    NoOp -> 
      model ! []

    StoryFailed id _ -> 
      model ! []

    StoryLoaded id item -> 
      let  
        updatedStories = List.map (\ story -> if story.id == id then {story | title = item.title } else story) model.stories
      in
        {model | stories = (item::model.stories) } ! []


-- VIEW 

storyView : Story -> Html Msg
storyView {title} = 
  div [] 
    [ p [] [ text title ] 
    ]

view : Model -> Html Msg
view model = 
  let 
    stories = List.map (\ story -> storyView story ) model.stories
  in
    div [] stories
