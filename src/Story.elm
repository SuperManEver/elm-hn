module Story exposing (..)

import Html exposing (Html, div, a, text, button, span)
import Html.Attributes  exposing (class, target, href)
import Html.Events exposing (onClick)

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


-- UPDATE 
type OutMsg 
  = SaveStory Int 
  | RemoveStory Int

type InternalMsg 
  = NoOp

type Msg 
  = ForSelf Int InternalMsg
  | ForParent OutMsg

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


update : InternalMsg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []


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
