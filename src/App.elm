import Html exposing (..)
import Html.App as App exposing (program)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (class, id, href)
import String exposing (concat)
import Http exposing (Error)
import Task exposing (Task, perform)
import Json.Decode as Json exposing ((:=))
import Dict exposing (Dict)

-- modules
import SideBar 
import StoryItem

-- routing 
import Navigation
import Router exposing (toHash, hashParser, pageParser, Route)

import Ports exposing (..)

latestURL : String 
latestURL = "https://hacker-news.firebaseio.com/v0/topstories.json"

shift : Int 
shift = 40


-- MAIN 
main = 
  program 
    { init = init 
    , update = update 
    , view = view 
    , subscriptions = subscriptions
    }  


-- ROUTER 
urlUpdate : Result String Route -> Model -> (Model, Cmd Msg)
urlUpdate result model =
  case Debug.log "result" result of
    Err _ ->
      ( model, Navigation.modifyUrl (toHash model.route) ) 

    Ok route ->
      { model | route = route } ! [ loadLatests ]


-- MODEL 
type alias Model = 
  { top_ids : List Int
  , top_stories : List StoryItem.Model
  , saved : List StoryItem.Model
  , sidebar : SideBar.Model
  , route : Route 
  , cache : Dict String (List String)
  }


defaultModel : Model 
defaultModel = 
  { top_ids     = []
  , top_stories = []
  , saved       = []
  , sidebar     = SideBar.defaultModel
  , route       = Router.defaultRoute
  , cache       = Dict.empty
  }  


-- INIT  
init : (Model, Cmd Msg)
init = 
  defaultModel ! [ loadLatests ]


-- COMMANDS
idsDecoder : Json.Decoder (List Int)
idsDecoder = 
  Json.list Json.int


loadLatests : Cmd Msg
loadLatests = 
  Task.perform LatestFailed LatestLoaded (Http.get idsDecoder latestURL) 


-- UPDATE 
type Msg 
  = NoOp
  | LatestFailed Http.Error
  | LatestLoaded (List Int)
  | StoryMsg StoryItem.InternalMsg 
  | LoadMoreStories
  | Scroll Bool
  | SideBarMsg SideBar.Msg 
  | SaveStory StoryItem.Model 
  | RemoveStory Int 


childTranslator : StoryItem.Translator Msg 
childTranslator = 
  StoryItem.translator 
    { onInternalMessage = StoryMsg
    , onSaveStory = SaveStory  -- App's level message
    , onRemoveStory = RemoveStory
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []


    LatestFailed error -> 
      model ! []


    LatestLoaded ids ->
      let 
        current       = List.take shift ids  -- I think this can be removed after I will change architecture
        top_stories'  = List.map StoryItem.createStory current
        top_ids'      = List.drop shift ids
      in
        { model | top_ids = top_ids' , top_stories = top_stories' } 
        !
        [ 
          current 
            |> StoryItem.loadStories
            |> Cmd.map StoryMsg 
        ]


    StoryMsg subMsg -> 
      let 
        (top_stories', cmd ) = StoryItem.update subMsg model.top_stories
      in
        { model | top_stories = top_stories' } 
        ! 
        [ Cmd.map childTranslator cmd ]


    -- possibly can create some abstraction on LatestLoaded & LoadMoreStories
    LoadMoreStories -> 
      let 
        current  = List.take shift model.top_ids
        top_ids' = List.drop shift model.top_ids
        top_stories' =
          current 
            |> List.map StoryItem.createStory
            |> (++) model.top_stories 

      in
        { model | top_ids = top_ids', top_stories = top_stories' } 
        ! 
        [ 
          current 
            |> StoryItem.loadStories 
            |> Cmd.map StoryMsg 
        ]
   

    Scroll pos -> 
      if pos 
      then update LoadMoreStories model 
      else update NoOp model


    SideBarMsg subMsg -> 
      let
        (updatedSidebar, sidebarCmd) = SideBar.update subMsg model.sidebar
      in 
        {model | sidebar = updatedSidebar} ! [ Cmd.map SideBarMsg sidebarCmd ]


    SaveStory story -> 
      let 
        top_stories'  = List.filter (\ s -> not (s.id == story.id)) model.top_stories
        saved'    = story::model.saved
      in
        { model | top_stories = top_stories', saved = saved' } ! []

    RemoveStory id -> 
      model ! []


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg 
subscriptions model = 
  scroll Scroll


-- VIEW 
view : Model -> Html Msg
view model = 
  let 
    sidebar = App.map SideBarMsg (SideBar.view model.sidebar)
  in
    div [] 
      [ sidebar 
      , storyList model
      ]


storyList : Model -> Html Msg 
storyList {top_stories} = 
  top_stories
    |> StoryItem.view 
    |> div [ class "main-container" ] 
    |> App.map childTranslator

