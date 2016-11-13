module StoryItem exposing (..)

import Html exposing (..)
import Task
import Json.Decode exposing (Decoder, list, int, string, object3, (:=))
import Http
import String exposing (concat)

-- MODEL 
type alias Model = 
  { id : Int 
  , title : String 
  , url : String
  }

type Msg 
  = NoOp
  | StoryFetchFail Http.Error
  | FetchSucceed Int Model


createStory : Int -> Model 
createStory id = 
  { id = id 
  , title = ""
  , url = "" 
  }


-- COMMAND 
loadStories : List Model -> List (Cmd Msg)
loadStories stories = 
  let 
    url = "https://hacker-news.firebaseio.com/v0/item/"

    fetchSucceed id story = FetchSucceed id story

    makeUrl id = 
      concat [url, toString id,".json"]

    decoder = 
        object3 Model 
          ("id" := int)
          ("title" := string)
          ("url" := string)

  in 
    List.map 
      (\ story -> Task.perform StoryFetchFail (fetchSucceed story.id) (Http.get decoder (makeUrl story.id)) ) 
      stories
    


-- UPDATE
update : Msg -> List Model -> (List Model, Cmd Msg)
update msg model =
  case msg of 
    NoOp ->
      model ! []

    StoryFetchFail _ -> 
      model ! []

    FetchSucceed id story -> 
      let 
        updateModel = 
          List.map 
            (\ s -> if s.id == id then {s | title = "Hello" } else s) 
            model
      in
        model ! []

-- VIEW 
storyView : Model -> Html Msg 
storyView {id, title} = 
  div [] 
    [ p [] [ text title ]
    ]


view : List Model -> Html Msg
view model = 
  let 
    stories = List.map (\ story -> storyView story) model
  in
    div [] stories
