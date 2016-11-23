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
-- latestURL = "https://hacker-news.firebaseio.com/v0/newstories.json"
latestURL = "https://hacker-news.firebaseio.com/v0/topstories.json"

shift : Int 
shift = 40

-- MAIN 
main = 
  Navigation.program (Navigation.makeParser hashParser)
  { init = init 
  , update = update 
  , urlUpdate = urlUpdate
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
  { storiesIds : List Int
  , stories : List StoryItem.Model
  , saved : List StoryItem.Model
  , sidebar : SideBar.Model
  , route : Route 
  , cache : Dict String (List String)
  }


defaultModel : Model 
defaultModel = 
  { storiesIds  = []
  , stories     = []
  , saved       = []
  , sidebar     = SideBar.defaultModel
  , route       = Router.defaultRoute
  , cache       = Dict.empty
  }  


-- INIT  
init : Result String Route -> (Model, Cmd Msg)
init result = 
  urlUpdate result (defaultModel) 


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
        current = List.take shift ids
        newStories = 
          List.map (\ id -> StoryItem.createStory id) current  -- posibly can rewrite
      in
        { model | storiesIds = (List.drop shift ids) , stories = newStories } 
        !
        [ Cmd.map StoryMsg (StoryItem.loadStories current) ]


    StoryMsg subMsg -> 
      let 
        (stories', cmd ) = StoryItem.update subMsg model.stories
      in
        { model | stories = stories' } ! [ Cmd.map childTranslator cmd ]


    -- possibly can create some abstraction on LatestLoaded & LoadMoreStories
    LoadMoreStories -> 
      let 
        current = List.take shift model.storiesIds
        newStories = 
          List.map (\ id -> StoryItem.createStory id) current
      in
        { model 
        | storiesIds = (List.drop shift model.storiesIds)
        , stories = model.stories ++ newStories
        } 
        ! [ Cmd.map StoryMsg (StoryItem.loadStories current)]
   

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
      model ! []


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
storyList {stories} = 
  stories
    |> StoryItem.view 
    |> div [ class "main-container" ] 
    |> App.map childTranslator

