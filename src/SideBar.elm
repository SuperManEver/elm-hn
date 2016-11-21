module SideBar exposing (..)

import Html exposing (Html, div, ul, li, a, text, aside, span)
import Html.Attributes exposing (class, id, href)
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
  let 
    isOpen = if state then "open" else ""
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
    div [ class (concat ["toggle-sidebar", " ", open]), onClick Toggle ] 
      [ span [ class "glyphicon glyphicon-align-justify" ] [] ]