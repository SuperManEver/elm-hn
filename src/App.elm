import Html exposing (..)
import Html.App as App exposing (program)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (class, id, href)
import String exposing (concat)
import Http exposing (Error)
import Task exposing (Task, perform)
import Json.Decode as Json exposing ((:=))
import StoryItem
import SideBar 
import Dict exposing (Dict)

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
  { storiesIds = []
  , stories = []
  , saved = []
  , sidebar = SideBar.defaultModel
  , route = Router.defaultRoute
  , cache = Dict.empty
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
  | StoryMsg StoryItem.Msg 
  | LoadMoreStories
  | Scroll Bool
  | SideBarMsg SideBar.Msg 


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
          List.map (\ id -> StoryItem.createStory id) current
      in
        {model 
        | storiesIds = (List.drop shift ids)
        , stories = newStories 
        } 
        ! [ Cmd.map StoryMsg (StoryItem.loadStories current) ]

    StoryMsg subMsg -> 
      case subMsg of
        StoryItem.SaveStory item -> 
          { model 
          | saved   = { item | saved = not item.saved }::model.saved
          , stories = List.filter (\ story -> not (story.id == item.id)) model.stories
          } ! [] 

        StoryItem.RemoveStory id -> 
          { model 
          | 
          saved = List.filter (\ story -> not (story.id == id)) model.saved
          } ! []

        _ -> 
          let 
            (updatedItems, storyItemCmd) = StoryItem.update subMsg model.stories
          in
            {model | stories = updatedItems} ! [ Cmd.map StoryMsg storyItemCmd ]


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
storyList model = 
  model 
    |> filterStories
    |> div [ class "main-container" ] 
    |> App.map StoryMsg


filterStories : Model -> List (Html StoryItem.Msg )
filterStories {stories, saved, route} = 
  case route of 
    Router.Home -> 
      StoryItem.view stories

    Router.Saved -> 
      StoryItem.view saved

