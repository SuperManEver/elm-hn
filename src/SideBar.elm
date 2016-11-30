module SideBar exposing (..)

import Html exposing (Html, div, ul, li, a, text, aside, span)
import Html.Attributes exposing (class, id, href, classList)
import Html.Events exposing (onClick)
import String exposing (concat)

-- MODEL 
type alias Model = 
  { state : Bool
  }

defaultModel : Model 
defaultModel = 
  { state = False 
  }

type alias Link = 
  ( String, String )


links : List Link
links = [ ("Top stories", "#/home"), ("Bookmarks", "#/saved") ]

-- UPDATE 
type Msg 
  = NoOp
  | Toggle

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    Toggle ->
      {model | state = not model.state} ! []


-- VIEW 
view : Model -> Html Msg 
view {state} = 
  aside [ id "sidebar", classList [ ("open", state) ] ] 
    [ (sidebarToggle state)
    , navigation links
    ]    


navigation : List Link -> Html Msg 
navigation links = 
  links 
    |> List.map linkView
    |> ul [ class "navigation" ]  


linkView : Link -> Html Msg 
linkView (title, url) = 
  li [] 
    [ a [ class "link", href url ] [ text title ] 
    ]


sidebarToggle : Bool -> Html Msg 
sidebarToggle open = 
    div 
      [ 
        classList [
          ("toggle-sidebar", True), 
          ("open", open)
        ], onClick Toggle 
      ] 
      [ span [ class "glyphicon glyphicon-align-justify" ] [] ]

