module Leftins where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import StartApp
import List exposing (map, map2)
import String exposing (toList, reverse)
import Char exposing (toCode, isDigit)


-----------------
-- math routines
-----------------

-- list of digits in a leftin, 1st item represents right-most digit
type alias Leftin = List Int

-- add two leftins in a given base
add : Leftin -> Leftin -> Int -> Leftin
add a b base =
  let rawsum = map2 (+) a b
      carries = 0 :: map (\x -> if x >= base then 1 else 0) rawsum
      carrysum = map2 (+) rawsum carries
    in map (\x -> if x >= base then x-base else x) carrysum

-- convert a digit (0-9) or letter (a-z -> 10-36) to an int
charToInt : Char -> Int
charToInt c =
  if (isDigit c)
    then (toCode c) - (toCode '0')
    else (toCode c) - (toCode 'a') + 10

-- convert a string to a list of ints representing a leftin
stringToLeftin : String -> Leftin
stringToLeftin s =
  let sList = toList (reverse s)
    in map (\c -> charToInt c) sList

-----------------
-- model
-----------------

type alias Model =
  { num1 : Leftin
  , num2 : Leftin
  , result : Leftin
  , counter : Int
  }

type Action
  = Update1 String
  | Update2 String
  | Add
  | Multiply

-----------------
-- main
-----------------

main =
  StartApp.start { model = model, view = view, update = update }

model : Model
model = { num1 = [ 1 ], num2 = [ 2 ], result = [], counter = 0 }

view address model =
  div []
    [
      button [ onClick address Add ] [ text "a + b" ]
    , button [ onClick address Multiply ] [ text "a * b" ]
    , div []
      [
        label [] [ text "a: ..." ]
      , input [ id "in-a", on "input" targetValue (Signal.message address << Update1) ] []
      ]
    , div []
      [
        label [] [ text "b: ..." ]
      , input [id "in-b", on "input" targetValue (Signal.message address << Update2)] []
      ]
    , div [] [ text (toString model) ]
    , div [] [ text (toString (add [3, 7, 4] [8, 6, 2] 10)) ]
    ]

update action model =
  case action of
    Update1 str -> { model | num1 <- stringToLeftin str }
    Update2 str -> { model | num2 <- stringToLeftin str }
    Add -> { model | result <- add model.num1 model.num2 10 }
    Multiply -> { model | counter <- model.counter - 1 }


{--
view address model =
  div []
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick address Increment ] [ text "+" ]
    , div [] [ text (toString (add [3, 7, 4] [8, 6, 2] 10)) ]
    ]

type Action = Increment | Decrement

update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1
--}
