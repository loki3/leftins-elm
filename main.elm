-----------------
-- Leftins UI
-- Scott Sherman
-- https://github.com/loki3/leftins-elm
-----------------

module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, targetValue)
import List
import String
import Leftins exposing (..)

-----------------
-- model & messages
-----------------

type alias Model =
  { num1 : Leftin
  , num2 : Leftin
  , base : Int
  , result : Leftin        -- when there's one result
  , results : List Leftin  -- when there are mutliple results
  , description : String
  }

type Msg
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

-- describe the operation and its result
describeOne : Msg -> Model -> Leftin -> String
describeOne msg m result =
  let n1 = leftinToString m.num1
      n2 = leftinToString m.num2
      r = " = " ++ leftinToString result
      b = "  (base " ++ toString m.base ++ ")"
  in case msg of
    None -> ""
    Add -> n1 ++ " + " ++ n2 ++ r ++ b
    Multiply -> n1 ++ " * " ++ n2 ++ r ++ b
    Power -> n1 ++ " ^ " ++ n2 ++ r ++ b
    _ -> toString msg

-- describe the operation and all its result
describeAll : Msg -> Model -> List Leftin -> String
describeAll msg m results =
  let n1 = leftinToString m.num1
      n2 = leftinToString m.num2
      r = " = " ++ toString (List.map leftinToString results)
      b = "  (base " ++ toString m.base ++ ")"
  in case msg of
    Root -> n2 ++ " -/ " ++ n2 ++ r ++ b
    _ -> toString msg

toIntWithDefault : Int -> String -> Int
toIntWithDefault default str =
  case String.toInt str of
    Ok value -> value
    Err err -> default

-----------------
-- main
-----------------

main =
  Html.beginnerProgram { model = model, view = view, update = update }

view : Model -> Html Msg
view model =
  div []
    [
      button [ onClick Add ] [ text "a + b" ]
    , button [ onClick Multiply ] [ text "a * b" ]
    , button [ onClick Power ] [ text "a ^ b" ]
    , button [ onClick Root ] [ text "b -/ a" ]
    , div []
      [
        label [] [ text "a: ..." ]
      , input [ onInput Update1 ] []
      ]
    , div []
      [
        label [] [ text "b: ..." ]
      , input [ onInput Update2 ] []
      ]
    , div []
      [
        label [] [ text "base:" ]
      , input [ onInput UpdateBase ] []
      ]
    , div [] [ text model.description ]
    , div [] [ text (toString model) ]
    ]

update message model =
  case message of
    Update1 str -> { model | num1 = stringToLeftin str }
    Update2 str -> { model | num2 = stringToLeftin str }
    UpdateBase str -> { model | base = toIntWithDefault 10 str }
    Add ->
      let answer = add model.base model.num1 model.num2
      in { model | result = answer
                 , description = describeOne message model answer }
    Multiply ->
      let answer = multiply model.base model.num1 model.num2
      in  { model | result = answer
                 , description = describeOne message model answer }
    Power ->
      let answer = power model.base model.num1 (leftinToInt model.base model.num2)
      in { model | result = answer
                 , description = describeOne message model answer }
    Root ->
      let answer = findRoots model.base model.num1 (leftinToInt model.base model.num2)
      in { model | results = answer
                 , description = describeAll message model answer }
    None -> model
