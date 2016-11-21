import Html exposing (..)
import Html.App as App exposing (program)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (class, id, href)
import String exposing (concat)
import Http exposing (Error)
import Task exposing (Task, perform)
import Json.Decode as Json exposing ((:=))
import StoryItem
import Window exposing (height)
import Basics.Extra exposing (never)
import String exposing (concat)

import Ports exposing (..)

latestURL : String 
-- latestURL = "https://hacker-news.firebaseio.com/v0/newstories.json"
latestURL = "https://hacker-news.firebaseio.com/v0/topstories.json"

shift : Int 
shift = 40

-- MAIN 
main = program
  { init = init 
  , update = update 
  , view = view 
  , subscriptions = subscriptions
  }




-- MODEL 
type alias Model = 
  { storiesIds : List Int
  , stories : List StoryItem.Model
  , sidebarState : Bool
  }

defaultModel : Model 
defaultModel = 
  { storiesIds = []
  , stories = []
  , sidebarState = True
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
  | StoryMsg StoryItem.Msg 
  | LoadMoreStories
  | GetWindowHeight
  | WindowHeight Int
  | Scroll Bool
  | ToggleSidebar

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

    WindowHeight height -> 
      model ! []

    GetWindowHeight -> 
      model ! [perform never WindowHeight Window.height]

    Scroll pos -> 
      if pos 
      then update LoadMoreStories model 
      else update NoOp model

    ToggleSidebar -> 
      { model | sidebarState = not model.sidebarState } ! []


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg 
subscriptions model = 
  scroll Scroll


view : Model -> Html Msg
view model = 
  let 
    stories = App.map StoryMsg (StoryItem.view model.stories)
  in
    div [] 
      [ sidebar model
      , div [ class "main-container" ] [ stories ]
      ]

sidebar : Model -> Html Msg 
sidebar {sidebarState} = 
  let 
    isOpen = if sidebarState then "open" else ""
  in
    aside [ id "sidebar", class isOpen ] 
      [ (sidebarToggle isOpen)
      , navigation
      ]    

navigation : Html Msg 
navigation = 
  ul [ class "navigation" ] 
    [ li [] 
      [ a [ class "link", href "#home" ] [ text "Top stories" ]
      , a [ class "link", href "#saved" ] [ text "Bookmarks" ]
      ]
    ]      

sidebarToggle : String -> Html Msg 
sidebarToggle open = 
    div [ class (concat ["toggle-sidebar", " ", open]), onClick ToggleSidebar ] 
      [ span [ class "glyphicon glyphicon-align-justify" ] [] ]
