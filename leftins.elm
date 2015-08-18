-----------------
-- Leftins routines
-- Scott Sherman
-- https://github.com/loki3/leftins-elm
-----------------

module Leftins
  ( Leftin
  , normalize, add, multiply, power
  , stringToLeftin, leftinToString, leftinToInt
  ) where

import List exposing (map, map2, append, indexedMap, foldl)
import String exposing (toList, fromList, reverse)
import Char exposing (toCode, fromCode, isDigit)


-----------------
-- math routines
-----------------

-- list of digits in a leftin, 1st item represents right-most digit
type alias Leftin = List Int

-- take a list of numbers & reduce them to valid
-- digits in the given base, applying carries
normalize : Int -> Leftin -> Int -> Leftin
normalize base xs carry =
  case xs of
    x::xend ->
      let sum = x + carry
          newcarry = sum // base
          digit = sum % base
      in digit :: (normalize base xend newcarry)
    [] -> []

-- add two leftins in a given base
add : Int -> Leftin -> Leftin -> Leftin
add base a b =
  let rawsum = map2 (+) a b
  in normalize base rawsum 0

-- multiply two leftins together, recursively multiplying each
-- digit from the first against the second.
-- 'prefix' is a series of 0s added to each product to
-- simulate shifting the position of the digit we're multiplying.
-- unnormalized
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
  in normalize base rawProduct 0

-- a^n recursively, stopping when n decreases to 1
-- unnormalized
powerRecurse : Leftin -> Leftin -> Int -> Leftin
powerRecurse a current n =
  case n of
    0 -> [1]
    1 -> a
    _ -> multiplyRecurse a (powerRecurse a current (n-1)) []
-- a^n in the given base
power : Int -> Leftin -> Int -> Leftin
power base a n =
  let rawPower = powerRecurse a a n
  in normalize base rawPower 0

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

calc base position digit = digit * base ^ position

-- convert from a list of ints representing a leftin to an int
leftinToInt : Int -> Leftin -> Int
leftinToInt base a =
  indexedMap (calc base) a |> foldl (+) 0
