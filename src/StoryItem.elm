module StoryItem exposing (..)

import Html exposing (Html, div, a, text, button, span)
import Html.Attributes as Attr exposing (class, target)
import Html.Events exposing (onClick)
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
  , saved : Bool
  }

createStory : Int -> Model
createStory id = 
  { id = id
  , title = "Loading"
  , url = ""
  , saved = False
  }

defaultModel : Model 
defaultModel = 
  { id = 1 
  , title = ""
  , url = ""
  , saved = False
  }


-- COMMANDS
decoder : Json.Decoder Model
decoder = 
  Json.object4 Model
    ("id" := Json.int)
    ("title" := Json.string)
    ("url" := Json.string)
    (Json.succeed False)


itemLoadTask : Int -> Task Error Model
itemLoadTask id = 
  Http.get decoder <| concat [ itemUrl, toString id, ".json" ]
 

loadStory : Int -> Cmd Msg 
loadStory id = 
  id 
    |> itemLoadTask
    |> perform (StoryFailed id) (StoryLoaded id) 


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
  | SaveStory Model
  | RemoveStory Int


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
          stories = updateModel (\ val -> item ) id model
        in
          stories ! []

      SaveStory item -> 
        model ! [] -- intercepted by parent

      RemoveStory id -> 
        model ! []


-- VIEW 
viewItem : Model -> Html Msg
viewItem story = 
  let 
    action = 
      if story.saved 
      then span [ class "glyphicon glyphicon-trash pull-right", onClick (RemoveStory story.id) ] []
      else span [ class "glyphicon glyphicon-bookmark pull-right", onClick (SaveStory story) ] []

  in 
    div [ class "story-item" ] 
      [ a [ target "_blank", Attr.href story.url ] [ text story.title ] 
      , action
      ]

view : List Model -> List (Html Msg)
view stories = 
  stories 
    |> List.map viewItem

