module StoryManager exposing (..)

import Html exposing (Html, div, a, text, button, span)
import Html.Attributes as Attr exposing (class, target)
import Html.Events exposing (onClick)
import Html.App as App
import Html.Lazy exposing (lazy, lazy2)
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


-- UPDATE 
type Msg 
  = NoOp 
  | LatestFailed Http.Error
  | LatestLoaded (List Int)
  | LoadMoreStories 
  | Scroll Bool
  | SaveStory Int 
  | RemoveStory Int
  | StoryMsg Int Story.InternalMsg


storyTranslator : Story.Tranlator Msg
storyTranslator = 
  Story.translator 
    { onInternalMessage = StoryMsg
    , onSaveStory = SaveStory
    , onRemoveStory = RemoveStory
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  let 
    createStoryStub =
      Story.createStory 

    preloadStories ids = 
      let 
        top_ids'        = List.drop shift ids
        top_stories'    = List.take shift ids
        cached_stories' = 
          List.foldl 
            (\ id acc -> Dict.insert id (createStoryStub id) acc)
            model.cached_stories 
            top_stories'
      in
        { model 
        | top_ids = top_ids'
        , top_stories = model.top_stories ++ top_stories'
        , cached_stories = cached_stories' } 
        ! 
        [ loadStories top_stories' ]

  in
    case msg of 
      NoOp -> 
        model ! []


      LatestFailed error -> 
        model ! []


      LatestLoaded ids ->
        preloadStories ids 


      LoadMoreStories -> 
        preloadStories model.top_ids

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
              cached_stories' = Dict.update id (Maybe.map (\ s -> story')) model.cached_stories
            in 
              { model | cached_stories = cached_stories' } ! [ Cmd.map storyTranslator cmd ]

          Nothing -> 
            model ! []


-- VIEW 
view : Model -> Html Msg
view model = 
  let 
    collect = (\ curr acc -> 
          case curr of 
            Just item -> item::acc 
            Nothing -> acc)  

    stories = 
      model.top_stories
        |> List.map (\ id -> Dict.get id model.cached_stories) 
        |> List.foldr collect []
        |> List.map (lazy <| Story.view)
  in 
    lazy2 div [ class "main-container" ] stories |> App.map storyTranslator


-- COMMANDS

loadStories : List Int -> Cmd Msg
loadStories ids = 
  ids
    |> List.map Story.loadStoryTask
    |> Cmd.batch 
    |> Cmd.map storyTranslator


-- load ids 
idsDecoder : Json.Decoder (List Int)
idsDecoder = 
  Json.list Json.int


loadLatests : Cmd Msg
loadLatests = 
  Task.perform LatestFailed LatestLoaded (Http.get idsDecoder latestURL) 




