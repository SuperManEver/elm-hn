module StoryItem exposing (..)

import Html exposing (..)

-- MODEL 
type alias Model = 
  { id : Int 
  , title : String 
  , url : String
  }

type Msg = NoOp

createStory : Int -> Model 
createStory id = 
  { id = id 
  , title = ""
  , url = "" 
  }

-- COMMAND 
loadStories : List Int -> Cmd Msg
loadStories ids = 
  -- for each id perform HTTP fetch of story
  Cmd.none

-- UPDATE


-- VIEW 
storyView : Model -> Html Msg 
storyView {id} = 
  div [] 
    [ p [] [ text <| toString id ] 
    ]


view : List Int -> Html Msg
view model = 
  let 
    stories = 
      model 
        |> List.map createStory
        |> List.map (\ s -> storyView s) 
  in
    div [] 
      [ div [] stories
      ]