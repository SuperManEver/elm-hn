module Counter exposing (..)

import Html exposing (Html, text, p, div, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

-- MODEL 
type alias Model = 
  { id : Int 
  , value : Int 
  }

type Msg 
  = NoOp
  | Increment Int
  | Decrement Int

-- UPDATE
update : Msg -> List Model -> (List Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    Increment id -> 
      let 
        counters = 
          List.map 
            (\ c -> if c.id == id then {c | value = c.value + 1 } else c) 
            model
      in
        counters ! []

    Decrement id -> 
      let 
        counters = 
          List.map 
            (\ c -> if c.id == id then {c | value = c.value - 1 } else c) 
            model
      in
        counters ! []


-- VIEW
viewCounter : Model -> Html Msg 
viewCounter {id, value} = 
  div [ class "counter" ] 
    [ p [] [ text <| toString value ]
    , button [ onClick (Increment id) ] [ text "+" ]
    , button [ onClick (Decrement id) ] [ text "-" ]
    ]

view : List Model -> Html Msg 
view model = 
  let 
    counters = List.map (\ c -> viewCounter c) model
  in 
    div [] counters
