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

import Ports exposing (..)

-- MAIN 
main = 
  program 
    { init = init 
    , update = update 
    , view = view 
    , subscriptions = subscriptions
    }  


-- MODEL 
type alias Model = 
  { sidebar : SideBar.Model
  , stories : StoryItem.Model
  }


defaultModel : Model 
defaultModel = 
  { sidebar = SideBar.defaultModel
  , stories = StoryItem.initModel
  }  


-- INIT  
init : (Model, Cmd Msg)
init = 
  defaultModel ! [ Cmd.map StoryMsg StoryItem.loadLatests ]


-- UPDATE 
type Msg 
  = NoOp
  | StoryMsg StoryItem.Msg 
  | Scroll Bool
  | SideBarMsg SideBar.Msg 

{--
> doMsg = Task.succeed >> flip Task.perform
<function> : a -> (a -> msg) -> Platform.Cmd.Cmd msg

> doMsg = Task.succeed >> Task.perform identity
<function> : a -> Platform.Cmd.Cmd a
--}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    StoryMsg subMsg -> 
      let 
        (stories', cmd ) = StoryItem.update subMsg model.stories
      in
        { model | stories = stories' } ! [ Cmd.map StoryMsg cmd ]


    Scroll pos -> 
      let
        (stories', cmd) = StoryItem.update (StoryItem.Scroll pos) model.stories
      in 
        { model | stories = stories' } ! [ Cmd.map StoryMsg cmd ]


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
    stories = App.map StoryMsg (StoryItem.view model.stories)
  in
    div [] 
      [ sidebar 
      , stories
      ]


