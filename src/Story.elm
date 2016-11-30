module Story exposing (..)

import Html exposing (Html, div, a, text, button, span)
import Html.Attributes  exposing (class, target, href, title, classList)
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
  , read : Bool
  }

type alias StoryForLoad = 
  { id : Int 
  , title : String 
  , url : String
  }

createStory : Int -> Model
createStory id = 
  { id = id
  , title = "Loading"
  , url = ""
  , saved = False
  , read = False
  }

defaultModel : Model 
defaultModel = 
  { id = 1 
  , title = ""
  , url = ""
  , saved = False
  , read = False
  }


-- TRANSLATOR 

type alias TranslationDictionary parentMsg = 
  { onInternalMessage : Int -> InternalMsg -> parentMsg 
  , onSaveStory : Int -> parentMsg
  , onRemoveStory : Int -> parentMsg
  , onRemoveSaved : Int -> parentMsg
  }

type alias Tranlator parentMsg = 
  Msg -> parentMsg 

translator : TranslationDictionary parentMsg -> Tranlator parentMsg 
translator { onInternalMessage, onSaveStory, onRemoveStory, onRemoveSaved } msg = 
  case msg of 
    ForSelf id internal -> 
      onInternalMessage id internal

    ForParent (SaveStory id) -> 
      onSaveStory id 

    ForParent (RemoveStory id) -> 
      onRemoveStory id

    ForParent (RemoveSavedStory id) -> 
      onRemoveSaved id


-- UPDATE 
type OutMsg 
  = SaveStory Int 
  | RemoveStory Int
  | RemoveSavedStory Int

type InternalMsg 
  = NoOp
  | StoryLoaded StoryForLoad 
  | StoryFailed Error
  | MarkAsUnread

type Msg 
  = ForSelf Int InternalMsg
  | ForParent OutMsg


update : InternalMsg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    StoryLoaded st -> 
      { model | title = st.title, url = st.url } ! []

    StoryFailed err -> 
      { model | title = "Not Loading" } ! []

    MarkAsUnread -> 
      { model | read = not model.read } ! []


-- VIEW 
view : Model -> Html Msg
view story = 
  let 
    action = 
      if story.saved 
      then itemSavedView story
      else itemView story
  in 
    div [ class "story-item" ] 
      [ a [ target "_blank"
          , href story.url
          , classList [ ("readed", story.read) ] 
          ] 
          [ text story.title ] 
      , action
      ]


itemView : Model -> Html Msg 
itemView {id} = 
  div [ class "story-controls pull-right" ]
    [ span [ class "glyphicon glyphicon-bookmark", title "Save for later", onClick (ForParent <| SaveStory id) ] []
    , span [ class "glyphicon glyphicon-ok", title "Mark as read", onClick (MarkAsUnread |> ForSelf id) ] []
    , span [ class "glyphicon glyphicon-remove", title "Mark as read and hide", onClick (ForParent <| RemoveStory id) ] []
    ]

itemSavedView : Model -> Html Msg 
itemSavedView {id} = 
  div [ class "story-controls pull-right" ] 
    [ span [ class "glyphicon glyphicon-trash ", onClick (ForParent <| RemoveSavedStory id) ] []
    ]
  

-- COMMANDS 
storyDecoder : Json.Decoder StoryForLoad
storyDecoder = 
  Json.object3 StoryForLoad
    ("id" := Json.int)
    ("title" := Json.string)
    ("url" := Json.string)


itemLoadTask : Int -> Task Error StoryForLoad
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
