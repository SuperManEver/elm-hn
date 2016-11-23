module StoryItem exposing (..)

import Html exposing (Html, div, a, text, button, span)
import Html.Attributes as Attr exposing (class, target)
import Html.Events exposing (onClick)
import Task exposing (Task, perform)
import Json.Decode as Json exposing ((:=))
import Http exposing (Error)
import String exposing (concat)
import Dict exposing (Dict)
import Task exposing (perform)

itemUrl : String 
itemUrl = "https://hacker-news.firebaseio.com/v0/item/"

latestURL : String 
latestURL = "https://hacker-news.firebaseio.com/v0/topstories.json"


shift : Int 
shift = 40


-- MODEL 
type alias Story = 
  { id : Int 
  , title : String 
  , url : String
  , saved : Bool
  }


type alias Model = 
  { top_ids : List Int
  , top_stories : List Int
  , saved_stories : List Int
  , cached_stories : Dict Int Story
  }  


initModel : Model 
initModel = 
  { top_ids = []
  , top_stories = []
  , saved_stories = []
  , cached_stories = Dict.empty
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

-- load individual stories
storyDecoder : Json.Decoder Story
storyDecoder = 
  Json.object4 Story
    ("id" := Json.int)
    ("title" := Json.string)
    ("url" := Json.string)
    (Json.succeed False)


itemLoadTask : Int -> Task Error Story
itemLoadTask id = 
  Http.get storyDecoder <| concat [ itemUrl, toString id, ".json" ]
 

loadStory : Int -> Cmd Msg
loadStory id = 
  id 
    |> itemLoadTask
    |> perform (StoryFailed id) (StoryLoaded id) 


loadStories : List Int -> Cmd Msg
loadStories ids = 
  ids
    |> List.map loadStory
    |> Cmd.batch 


-- load ids 
idsDecoder : Json.Decoder (List Int)
idsDecoder = 
  Json.list Json.int


loadLatests : Cmd Msg
loadLatests = 
  Task.perform LatestFailed LatestLoaded (Http.get idsDecoder latestURL) 


-- UPDATE 
type Msg 
  = NoOp 
  | StoryFailed Int Http.Error 
  | StoryLoaded Int Story
  | LatestFailed Http.Error
  | LatestLoaded (List Int)
  | LoadMoreStories Bool
  | SaveStory Int 
  | RemoveStory Int


update : Msg -> Model -> (Model, Cmd Msg)
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
          cached' = Dict.insert id item model.cached_stories
        in
          { model | cached_stories = cached' } ! []


      LatestFailed error -> 
        model ! []


      LatestLoaded ids ->
        let 
          top_stories'  = List.take shift ids
          top_ids'      = List.drop shift ids
        in
          { model | top_ids = top_ids' , top_stories = top_stories' } 
          ! 
          [ loadStories top_stories' ]


      LoadMoreStories val -> 
        if val 
        then 
          let 
            top_ids'      = List.drop shift model.top_ids
            top_stories'  = List.take shift model.top_ids
          in
            { model | top_ids = top_ids', top_stories = model.top_stories ++ top_stories' } 
            ! 
            [ loadStories top_stories' ]
        else 
          model ! []


      SaveStory id -> 
        model ! []


      RemoveStory id -> 
        model ! []


-- VIEW 
view : Model -> Html Msg
view model = 
  let 
    f = (\ curr acc -> 
          case curr of 
            Just item -> item::acc 
            Nothing -> acc)
  in 
    model.top_stories
      |> List.map (\ id -> Dict.get id model.cached_stories) 
      |> List.foldr f []
      |> List.map viewItem 
      |> div [ class "main-container" ] 


viewItem : Story -> Html Msg
viewItem story = 
  let 
    action = 
      if story.saved 
      then 
        span 
          [ class "glyphicon glyphicon-trash pull-right"
          , onClick (RemoveStory story.id) 
          ] []

      else 
        span 
          [ class "glyphicon glyphicon-bookmark pull-right"
          , onClick (SaveStory story.id) 
          ] []

  in 
    div [ class "story-item" ] 
      [ a [ target "_blank", Attr.href story.url ] [ text story.title ] 
      , action
      ]



