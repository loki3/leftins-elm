-----------------
-- Leftins UI
-- Scott Sherman
-- https://github.com/loki3/leftins-elm
-----------------

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import StartApp
import Leftins exposing (..)

-----------------
-- model & actions
-----------------

type alias Model =
  { num1 : Leftin
  , num2 : Leftin
  , base : Int
  , result : Leftin
  , description : String
  }

type Action
  = None
  | Update1 String
  | Update2 String
  | Add
  | Multiply
  | Power

model : Model
model =
  { num1 = [ 7, 3, 6 ]
  , num2 = [ 5, 9, 6  ]
  , base = 10
  , result = []
  , description = ""
  }

-- describe the operation and its results
describe : Action -> Model -> Leftin -> String
describe a m result =
  let n1 = leftinToString m.num1
      n2 = leftinToString m.num2
      r = " = " ++ leftinToString result
      b = "  (base " ++ toString m.base ++ ")"
  in case a of
    None -> ""
    Add -> n1 ++ " + " ++ n2 ++ r ++ b
    Multiply -> n1 ++ " * " ++ n2 ++ r ++ b
    Power -> n1 ++ " ^ " ++ n2 ++ r ++ b

-----------------
-- main
-----------------

main =
  StartApp.start { model = model, view = view, update = update }

view address model =
  div []
    [
      button [ onClick address Add ] [ text "a + b" ]
    , button [ onClick address Multiply ] [ text "a * b" ]
    , button [ onClick address Power ] [ text "a ^ b" ]
    , div []
      [
        label [] [ text "a: ..." ]
      , input [ on "input" targetValue (Signal.message address << Update1) ] []
      ]
    , div []
      [
        label [] [ text "b: ..." ]
      , input [ on "input" targetValue (Signal.message address << Update2) ] []
      ]
    , div [] [ text model.description ]
    , div [] [ text (toString model) ]
    ]

update action model =
  case action of
    Update1 str -> { model | num1 <- stringToLeftin str }
    Update2 str -> { model | num2 <- stringToLeftin str }
    Add ->
      let answer = add model.base model.num1 model.num2
      in { model | result <- answer
                 , description <- describe action model answer }
    Multiply ->
      let answer = multiply model.base model.num1 model.num2
      in  { model | result <- answer
                 , description <- describe action model answer }
    Power ->
      let answer = power model.base model.num1 (leftinToInt model.base model.num2)
      in { model | result <- answer
                 , description <- describe action model answer }
