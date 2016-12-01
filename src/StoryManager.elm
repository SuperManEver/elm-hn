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
  | SaveStory Int Bool
  | RemoveStory Int
  | RemoveSavedStory Int
  | StoryMsg Int Story.InternalMsg


storyTranslator : Story.Tranlator Msg
storyTranslator = 
  Story.translator 
    { onInternalMessage = StoryMsg
    , onSaveStory = SaveStory
    , onRemoveStory = RemoveStory
    , onRemoveSaved = RemoveSavedStory 
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

      SaveStory id bool -> 
        let 
          saved' = 
            if bool 
            then model.saved_stories ++ [id]
            else List.filter (\ i -> not (i == id)) model.saved_stories

          cached_stories' = 
            Dict.update 
              id 
              (Maybe.map (\ story -> {story | saved = bool} )) 
              model.cached_stories
        in 
          { model | saved_stories = saved', cached_stories = cached_stories' } 
          ! 
          []

      RemoveStory id -> 
        let 
          top_stories'    = List.filter (\ d -> not (d == id)) model.top_stories
          -- saved_stories'  = List.filter (\ d -> not (d == id)) model.saved_stories
        in
          { model | top_stories = top_stories'} ! []

      RemoveSavedStory id -> 
        let 
          saved_stories'  = List.filter (\ i -> i /= id) model.saved_stories
          cached_stories' = Dict.update id (Maybe.map (\ s -> {s | saved = False })) model.cached_stories
        in
          { model | saved_stories = saved_stories', cached_stories = cached_stories' } 
          ! 
          []


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
view : Model -> String -> Html Msg
view model currentPage = 
  let 
    collect = (\ curr acc -> 
          case curr of 
            Just item -> item::acc 
            Nothing -> acc)  

    currentStories = 
      case currentPage of 
        "Top Stories" -> 
          model.top_stories

        "Bookmarks" -> 
          model.saved_stories 

        _ -> 
          model.top_stories

    stories = 
      currentStories
        |> List.map (\ id -> Dict.get id model.cached_stories) 
        |> List.foldr collect []
        |> List.map (lazy <| Story.view currentPage)
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




