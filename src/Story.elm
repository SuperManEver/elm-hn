module Story exposing (..)

import Html exposing (Html, div, a, text, button, span)
import Html.Attributes  exposing (class, target, href)
import Html.Events exposing (onClick)
import Http exposing (Error)
import Json.Decode as Json exposing ((:=))
import String exposing (concat)
import Task exposing (Task, perform)

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


-- TRANSLATOR 

type alias TranslationDictionary parentMsg = 
  { onInternalMessage : Int -> InternalMsg -> parentMsg 
  , onSaveStory : Int -> parentMsg
  , onRemoveStory : Int -> parentMsg
  }

type alias Tranlator parentMsg = 
  Msg -> parentMsg 

translator : TranslationDictionary parentMsg -> Tranlator parentMsg 
translator { onInternalMessage, onSaveStory, onRemoveStory } msg = 
  case msg of 
    ForSelf id internal -> 
      onInternalMessage id internal

    ForParent (SaveStory id) -> 
      onSaveStory id 

    ForParent (RemoveStory id) -> 
      onRemoveStory id


-- UPDATE 
type OutMsg 
  = SaveStory Int 
  | RemoveStory Int

type InternalMsg 
  = NoOp
  | StoryLoaded Model 
  | StoryFailed Error

type Msg 
  = ForSelf Int InternalMsg
  | ForParent OutMsg


update : InternalMsg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    StoryLoaded model -> 
      model ! []

    StoryFailed err -> 
      { model | title = "Not Loading" } ! []


-- VIEW 
view : Model -> Html Msg
view story = 
  let 
    action = 
      if story.saved 
      then itemView story
      else itemView story
  in 
    div [ class "story-item" ] 
      [ a [ target "_blank", href story.url ] [ text story.title ] 
      , action
      ]


itemView : Model -> Html Msg 
itemView {id} = 
  span 
    [ class "glyphicon glyphicon-bookmark pull-right"
    , onClick (ForParent <| SaveStory id)
    ] []


itemSavedView : Model -> Html Msg 
itemSavedView {id} = 
  span 
    [ class "glyphicon glyphicon-trash pull-right"
    , onClick (ForParent <| RemoveStory id)
    ] []


-- COMMANDS 
storyDecoder : Json.Decoder Model
storyDecoder = 
  Json.object4 Model
    ("id" := Json.int)
    ("title" := Json.string)
    ("url" := Json.string)
    (Json.succeed False)


itemLoadTask : Int -> Task Error Model
itemLoadTask id = 
  Http.get storyDecoder <| concat [ itemUrl, toString id, ".json" ]
 

loadStoryTask : Int -> Cmd Msg
loadStoryTask id = 
  let 
    onFail err =
      err
        |> StoryFailed 
        |> ForSelf id 

    onLoad story = 
      story 
        |> StoryLoaded 
        |> ForSelf id
  in
    id 
      |> itemLoadTask
      |> perform onFail onLoad 
