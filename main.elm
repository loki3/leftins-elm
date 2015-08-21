-----------------
-- Leftins UI
-- Scott Sherman
-- https://github.com/loki3/leftins-elm
-----------------

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import StartApp
import List
import String
import Leftins exposing (..)

-----------------
-- model & actions
-----------------

type alias Model =
  { num1 : Leftin
  , num2 : Leftin
  , base : Int
  , result : Leftin        -- when there's one result
  , results : List Leftin  -- when there are mutliple results
  , description : String
  }

type Action
  = None
  | Update1 String
  | Update2 String
  | UpdateBase String
  | Add
  | Multiply
  | Power
  | Root

model : Model
model =
  { num1 = [ 7, 3, 6 ]
  , num2 = [ 5, 9, 6  ]
  , base = 10
  , result = []
  , results = []
  , description = ""
  }

-----------------
-- support
-----------------

-- describe the operation and its results
describeOne : Action -> Model -> Leftin -> String
describeOne a m result =
  let n1 = leftinToString m.num1
      n2 = leftinToString m.num2
      r = " = " ++ leftinToString result
      b = "  (base " ++ toString m.base ++ ")"
  in case a of
    None -> ""
    Add -> n1 ++ " + " ++ n2 ++ r ++ b
    Multiply -> n1 ++ " * " ++ n2 ++ r ++ b
    Power -> n1 ++ " ^ " ++ n2 ++ r ++ b

describeAll : Action -> Model -> List Leftin -> String
describeAll a m results =
  let n1 = leftinToString m.num1
      n2 = leftinToString m.num2
      r = " = " ++ toString (List.map leftinToString results)
      b = "  (base " ++ toString m.base ++ ")"
  in case a of
    Root -> n2 ++ " -/ " ++ n2 ++ r ++ b

toIntWithDefault : Int -> String -> Int
toIntWithDefault default str =
  case String.toInt str of
    Ok value -> value
    Err err -> default

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
    , button [ onClick address Root ] [ text "b -/ a" ]
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
    , div []
      [
        label [] [ text "base:" ]
      , input [ on "input" targetValue (Signal.message address << UpdateBase) ] []
      ]
    , div [] [ text model.description ]
    , div [] [ text (toString model) ]
    ]

update action model =
  case action of
    Update1 str -> { model | num1 <- stringToLeftin str }
    Update2 str -> { model | num2 <- stringToLeftin str }
    UpdateBase str -> { model | base <- toIntWithDefault 10 str }
    Add ->
      let answer = add model.base model.num1 model.num2
      in { model | result <- answer
                 , description <- describeOne action model answer }
    Multiply ->
      let answer = multiply model.base model.num1 model.num2
      in  { model | result <- answer
                 , description <- describeOne action model answer }
    Power ->
      let answer = power model.base model.num1 (leftinToInt model.base model.num2)
      in { model | result <- answer
                 , description <- describeOne action model answer }
    Root ->
      let answer = findRoots model.base model.num1 (leftinToInt model.base model.num2)
      in { model | results <- answer
                 , description <- describeAll action model answer }
