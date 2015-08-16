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
import List exposing (map, map2, append)
import String exposing (toList, fromList, reverse)
import Char exposing (toCode, fromCode, isDigit)


-----------------
-- math routines
-----------------

-- list of digits in a leftin, 1st item represents right-most digit
type alias Leftin = List Int

-- take a list of numbers & reduce them to valid
-- digits in the given base, applying carries
normalize : Leftin -> Int -> Int -> Leftin
normalize xs base carry =
  case xs of
    x::xend ->
      let sum = x + carry
          newcarry = sum // base
          digit = sum % base
      in digit :: (normalize xend base newcarry)
    [] -> []

-- add two leftins in a given base
add : Int -> Leftin -> Leftin -> Leftin
add base a b =
  let rawsum = map2 (+) a b
  in normalize rawsum base 0

-- multiply two leftins together, recursively multiplying each
-- digit from the first against the second.
-- 'prefix' is a series of 0s added to each product to
-- simulate shifting the position of the digit we're multiplying.
-- note that the results aren't normalized to a base.
multiplyRecurse : Leftin -> Leftin -> Leftin -> Leftin
multiplyRecurse xs b prefix =
  case xs of
    x::[] -> map ((*) x) b |> append prefix
    x::xend ->
        let digitProduct = map ((*) x) b |> append prefix
            recurse = multiplyRecurse xend b (0 :: prefix)
        in map2 (+) digitProduct recurse
-- multiply two leftins in a given base
multiply : Int -> Leftin -> Leftin -> Leftin
multiply base a b =
  let rawProduct = multiplyRecurse a b []
  in normalize rawProduct base 0

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
    Add -> { model | result <- add model.base model.num1 model.num2 }
    Multiply -> { model | result <- multiply model.base model.num1 model.num2 }
