-----------------
-- Leftins routines & UI
-- Scott Sherman
-- https://github.com/loki3/leftins-elm
-----------------

module Leftins where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import StartApp
import List exposing (map, map2)
import String exposing (toList, fromList, reverse)
import Char exposing (toCode, fromCode, isDigit)


-----------------
-- math routines
-----------------

-- list of digits in a leftin, 1st item represents right-most digit
type alias Leftin = List Int

-- add two leftins in a given base
addRecurse : Leftin -> Leftin -> Int -> Int -> Leftin
addRecurse xs ys base carry =
  case xs of
    x::xend ->
      case ys of
        y::yend ->
          let sum = x + y + carry
              newcarry = if sum >= base then 1 else 0
              digit = if sum >= base then (sum - base) else sum
          in digit :: (addRecurse xend yend base newcarry)
        [] -> []
    [] -> []
add : Leftin -> Leftin -> Int -> Leftin
add a b base =
  addRecurse a b base 0

-----------------
-- conversion routines
-----------------

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

-- convert an int (0-31) to a digit (0-9) or letter (a-z)
intToChar : Int -> Char
intToChar i =
  fromCode (i + (if (i < 10) then toCode '0' else -10 + toCode 'a'))

-- convert a list of ints representing a leftin to a string
leftinToString : Leftin -> String
leftinToString l =
  map intToChar l |> fromList |> reverse

-----------------
-- model & actions
-----------------

type alias Model =
  { num1 : Leftin
  , num2 : Leftin
  , base : Int
  , result : Leftin
  }

type Action
  = Update1 String
  | Update2 String
  | Add
  | Multiply

model : Model
model =
  { num1 = [ 7, 3, 6 ]
  , num2 = [ 5, 9, 6  ]
  , base = 10
  , result = []
  }

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
    , div [] [ text (toString model) ]
    , div [] [ text (leftinToString model.result) ]
    ]

update action model =
  case action of
    Update1 str -> { model | num1 <- stringToLeftin str }
    Update2 str -> { model | num2 <- stringToLeftin str }
    Add -> { model | result <- add model.num1 model.num2 model.base }
