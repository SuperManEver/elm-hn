module StoryManager exposing (..)

import Html exposing (Html, div, a, text, button, span)
import Html.Attributes as Attr exposing (class, target)
import Html.Events exposing (onClick)
import Html.App as App
import Task exposing (Task, perform)
import Json.Decode as Json exposing ((:=))
import Http exposing (Error)
import String exposing (concat)
import Dict exposing (Dict)

import Story

latestURL : String 
latestURL = "https://hacker-news.firebaseio.com/v0/topstories.json"


shift : Int 
shift = 40


-- MODEL 
type alias Model = 
  { top_ids : List Int
  , top_stories : List Int
  , saved_stories : List Int
  , cached_stories : Dict Int Story.Model
  }  


initModel : Model 
initModel = 
  { top_ids = []
  , top_stories = []
  , saved_stories = []
  , cached_stories = Dict.empty
  }  

-- COMMANDS

loadStories : List Int -> Cmd Msg
loadStories ids = 
  ids
    |> List.map Story.loadStory
    |> Cmd.batch 
    |> Cmd.map childTranslator


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
  | StoryLoaded Int Story.Model
  | LatestFailed Http.Error
  | LatestLoaded (List Int)
  | LoadMoreStories 
  | Scroll Bool
  | SaveStory Int 
  | RemoveStory Int
  | StoryMsg Int Story.InternalMsg


childTranslator : Story.Tranlator Msg
childTranslator = 
  Story.translator 
    { onInternalMessage = StoryMsg
    , onSaveStory = SaveStory
    , onRemoveStory = RemoveStory
    , onLoad = StoryLoaded
    , onFail = StoryFailed
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
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


    LoadMoreStories -> 
      let 
        top_ids'      = List.drop shift model.top_ids
        top_stories'  = List.take shift model.top_ids
      in
        { model | top_ids = top_ids', top_stories = model.top_stories ++ top_stories' } 
        ! 
        [ loadStories top_stories' ]

    Scroll val -> 
      if val 
      then update LoadMoreStories model 
      else update NoOp model

    SaveStory id -> 
      let 
        saved' = model.saved_stories ++ [id]
        top_stories' = List.filter (\ i -> i /= id) model.top_stories
      in 
        {model | saved_stories = saved', top_stories = top_stories' } ! []

    RemoveStory id -> 
      model ! []

    StoryMsg id subMsg -> 
      case (Dict.get id model.cached_stories) of 
        Just story -> 
          let 
            (story', cmd)   = Story.update subMsg story
            cached_stories' = Dict.insert id story' model.cached_stories
          in 
            { model | cached_stories = cached_stories' } ! [ Cmd.map childTranslator cmd ]

        Nothing -> 
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
      |> List.map Story.view 
      |> div [ class "main-container" ] 
      |> App.map childTranslator






