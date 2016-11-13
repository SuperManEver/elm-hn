import Html exposing (..)
import Html.App as App exposing (program)
import String exposing (concat)
import Http exposing (Error)
import Task exposing (Task, perform)
import Json.Decode as Json exposing ((:=))

itemUrl : String 
itemUrl = "https://hacker-news.firebaseio.com/v0/item/"

stories : List Int 
stories = 
  [ 12941832,12941824,12941820,12941790,12941757,12941726,12941715,12941691,12941689,12941673,12941671 ]

-- MAIN 
main = program
  { init = init 
  , update = update 
  , view = view 
  , subscriptions = \ _ -> Sub.none
  }

-- INIT 
init : (Model, Cmd Msg)
init = 
  defaultModel ! [ (loadStory 12940712) ]


-- MODEL 
type alias Model = 
  { id : Int 
  , title : String 
  , url : String
  }

defaultModel : Model 
defaultModel = 
  { id = 1 
  , title = ""
  , url = ""
  }

type Msg 
  = NoOp
  | StoryFailed Http.Error
  | StoryLoaded Model


-- COMMANDS
decoder : Json.Decoder Model
decoder = 
  Json.object3 Model 
    ("id" := Json.int)
    ("title" := Json.string)
    ("url" := Json.string)

item : Int -> Task Error Model
item id = 
  Http.get decoder <| concat [ itemUrl, toString id, ".json" ]

loadStory : Int -> Cmd Msg 
loadStory id = 
  perform StoryFailed StoryLoaded <| item id

-- UPDATE 
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    StoryFailed _ -> 
      model ! []

    StoryLoaded item -> 
      {model | title = item.title} ! []


-- VIEW 
view : Model -> Html Msg
view model = 
  div [] 
    [ p [] [text model.title ] 
    ]