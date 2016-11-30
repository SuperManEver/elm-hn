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

-- TRANSLATOR 
type alias Translator parentMsg = 
  Msg -> parentMsg

type alias TranslationDictionary parentMsg = 
  { onInternalMsg : InternalMsg -> parentMsg 
  , onPageChange : String -> parentMsg 
  }

-- UPDATE 
type InternalMsg 
  = NoOp 
  | Toggle

type OutMsg 
  = ChangePage String

type Msg 
  = ForSelf InternalMsg
  | ForParent OutMsg

translator : TranslationDictionary parentMsg -> Translator parentMsg
translator { onInternalMsg, onPageChange } msg = 
  case msg of 
    ForSelf internal -> 
      onInternalMsg internal

    ForParent (ChangePage page) -> 
      onPageChange page


update : InternalMsg -> Model -> (Model, Cmd Msg)
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
  li [ onClick (ChangePage title |> ForParent) ]  
    [ a [ class "link", href url ] [ text title ] 
    ]


sidebarToggle : Bool -> Html Msg 
sidebarToggle open = 
    div 
      [ 
        classList [
          ("toggle-sidebar", True), 
          ("open", open)
        ], onClick (Toggle |> ForSelf)
      ] 
      [ span [ class "glyphicon glyphicon-align-justify" ] [] ]

