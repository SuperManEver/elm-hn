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
import StoryManager

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
  , stories : StoryManager.Model
  , currentPage : String
  }


defaultModel : Model 
defaultModel = 
  { sidebar = SideBar.defaultModel
  , stories = StoryManager.initModel
  , currentPage = "Top stories"
  }  


-- INIT  
init : (Model, Cmd Msg)
init = 
  defaultModel 
  ! 
  [ Cmd.map StoriesMsg StoryManager.loadLatests ]


-- UPDATE 
type Msg 
  = NoOp
  | StoriesMsg StoryManager.Msg 
  | Scroll Bool
  | SideBarMsg SideBar.InternalMsg  
  | ChangePage String

{--
> doMsg = Task.succeed >> flip Task.perform
<function> : a -> (a -> msg) -> Platform.Cmd.Cmd msg

> doMsg = Task.succeed >> Task.perform identity
<function> : a -> Platform.Cmd.Cmd a
--}

sidebarTranslator : SideBar.Translator Msg 
sidebarTranslator = 
  SideBar.translator 
    { onInternalMsg = SideBarMsg
    , onPageChange = ChangePage 
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    StoriesMsg subMsg -> 
      let 
        (stories', cmd ) = StoryManager.update subMsg model.stories
      in
        { model | stories = stories' } ! [ Cmd.map StoriesMsg cmd ]


    Scroll pos -> 
      let
        (stories', cmd) = StoryManager.update (StoryManager.Scroll pos) model.stories
      in 
        { model | stories = stories' } ! [ Cmd.map StoriesMsg cmd ]


    SideBarMsg subMsg -> 
      let
        (updatedSidebar, sidebarCmd) = SideBar.update subMsg model.sidebar
      in 
        {model | sidebar = updatedSidebar} ! [ Cmd.map sidebarTranslator sidebarCmd ]

    ChangePage page -> 
      {model | currentPage = page} ! []


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg 
subscriptions model = 
  scroll Scroll


-- VIEW 
view : Model -> Html Msg
view model = 
  let 
    sidebar = App.map sidebarTranslator (SideBar.view model.sidebar)
    stories = App.map StoriesMsg (StoryManager.view model.stories)
  in
    div [] 
      [ sidebar 
      , stories
      ]


