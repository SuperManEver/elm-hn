import Html exposing (..)
import Html.App as App exposing (program)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Random
import Example

main = 
  program
    { init = init 
    , update = update
    , view = view 
    , subscriptions = \_ -> Sub.none
    }


-- MODEL 
type alias Model = 
  { counters : List Counter.Model
  } 


type Msg 
  = NoOp
  | CreateCounter
  | AddCounter Int
  | CounterMsg Counter.Msg
  | RemoveCounter


init : (Model, Cmd Msg)
init = 
  Model [Counter.Model 1 0, Counter.Model 2 0] ! []


-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    CounterMsg subMsg -> 
      let 
        (updatedCounters, counterCmd) = Counter.update subMsg model.counters
      in 
        {model | counters = updatedCounters} ! [Cmd.map CounterMsg counterCmd]

    AddCounter id -> 
      {model | counters = (Counter.Model id 0)::model.counters} ! []

    CreateCounter -> 
      model ! [Random.generate AddCounter (Random.int 1 1000000)]

    RemoveCounter -> 
      {model | counters = List.drop 1 model.counters} ! []


-- VIEW 
view : Model -> Html Msg 
view model = 
  let 
    counters = App.map CounterMsg (Counter.view model.counters)
  in 
    div [] 
      [ div [ class "controls" ] 
        [ button [ onClick CreateCounter ] [ text "Add" ]
        , button [ onClick RemoveCounter ] [ text "Remove" ]
        ]
      , counters
      ]
