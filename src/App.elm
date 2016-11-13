import Html exposing (..)
import Html.App as App exposing (program)
import Json.Decode exposing (Decoder, list, int)
import Http
import Task
import StoryItem

main = 
  program 
    { init = init 
    , update = update 
    , view = view 
    , subscriptions = \ _ -> Sub.none
    }


-- MODEL 
type alias Model =
  { storyIds : List Int 
  , stories : List StoryItem.Model
  }

type Msg 
  = NoOp
  | LoadAllIds 
  | FetchFail Http.Error
  | FetchSuccess (List Int)
  | StoryItemsMsg StoryItem.Msg


-- INIT
init : (Model, Cmd Msg)
init =  
  Model [] [] ! [ getAllIds ]


-- COMMANDS 
getAllIds : Cmd Msg 
getAllIds =
  let 
    url = "https://hacker-news.firebaseio.com/v0/newstories.json"
    decodeIds = list int
  in
    Task.perform FetchFail FetchSuccess (Http.get decodeIds url)


-- UPDATE 
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp ->
      model ! []

    LoadAllIds -> 
      model ! [ getAllIds ]

    FetchSuccess allIds -> 
      let 
        stories = List.map (\ id -> StoryItem.createStory id) (List.take 10 allIds)
      in
        { model | storyIds = List.drop 10 allIds, stories = stories } 
        ! 
        List.map (\ cmd -> Cmd.map StoryItemsMsg cmd) (StoryItem.loadStories stories)

    FetchFail _ -> 
      model ! []

    StoryItemsMsg subMsg -> 
      let 
        (updatedStories, storiesCmd) = StoryItem.update subMsg model.stories 
      in
        {model | stories = updatedStories} ! [Cmd.map StoryItemsMsg storiesCmd]


-- VIEW
view : Model -> Html Msg 
view model = 
  let 
    items = App.map StoryItemsMsg (StoryItem.view model.stories)
  in 
    div [] 
      [ items 
      ]
      


