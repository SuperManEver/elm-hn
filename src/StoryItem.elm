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
type alias Story = 
  { id : Int 
  , title : String 
  , url : String
  , saved : Bool
  }

createStory : Int -> Story
createStory id = 
  { id = id
  , title = "Loading"
  , url = ""
  , saved = False
  }

defaultModel : Story 
defaultModel = 
  { id = 1 
  , title = ""
  , url = ""
  , saved = False
  }


-- COMMANDS
decoder : Json.Decoder Story
decoder = 
  Json.object4 Story
    ("id" := Json.int)
    ("title" := Json.string)
    ("url" := Json.string)
    (Json.succeed False)


itemLoadTask : Int -> Task Error Story
itemLoadTask id = 
  Http.get decoder <| concat [ itemUrl, toString id, ".json" ]
 

loadStory : Int -> Cmd InternalMsg
loadStory id = 
  id 
    |> itemLoadTask
    |> perform (StoryFailed id) (StoryLoaded id) 


loadStories : List Int -> Cmd InternalMsg
loadStories ids = 
  ids
    |> List.map loadStory
    |> Cmd.batch 


-- UPDATE 
type OutMsg 
  = SaveStory Story 
  | RemoveStory Int 


type InternalMsg 
  = NoOp 
  | StoryFailed Int Http.Error 
  | StoryLoaded Int Story


type Msg 
  = ForSelf InternalMsg 
  | ForParent OutMsg 


type alias TranslationDictionary parentMsg = 
  { onInternalMessage : InternalMsg -> parentMsg 
  , onSaveStory : Story -> parentMsg 
  , onRemoveStory : Int -> parentMsg
  }

-- ????
type alias Translator parentMsg = 
  Msg -> parentMsg

-- ????
translator : TranslationDictionary parentMsg -> Translator parentMsg 
translator { onInternalMessage, onSaveStory, onRemoveStory } msg = 
  case msg of 
    ForSelf internal -> 
      onInternalMessage internal 

    ForParent (SaveStory item) -> 
      onSaveStory item 

    ForParent (RemoveStory id) -> 
      onRemoveStory id


update : InternalMsg -> List Story -> (List Story, Cmd Msg)
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


-- VIEW 
view : List Story -> List (Html Msg)
view stories = 
  stories 
    |> List.map viewItem
    

viewItem : Story -> Html Msg
viewItem story = 
  let 
    action = 
      if story.saved 
      then 
        span 
          [ class "glyphicon glyphicon-trash pull-right"
          , onClick (ForParent <| RemoveStory story.id) 
          ] []

      else 
        span 
          [ class "glyphicon glyphicon-bookmark pull-right"
          , onClick (ForParent <| SaveStory story) 
          ] []

  in 
    div [ class "story-item" ] 
      [ a [ target "_blank", Attr.href story.url ] [ text story.title ] 
      , action
      ]



