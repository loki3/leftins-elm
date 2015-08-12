-----------------
-- unit tests of the Leftins module
-----------------

import ElmTest.Test exposing (..)
import ElmTest.Assertion exposing (..)
import ElmTest.Runner.Element exposing (..)
import Leftins exposing (..)

-----------------
-- math routines
-----------------

normalizeTest = suite "normalize"
  [ defaultTest (assertEqual [ 1 ] (normalize [ 1 ] 10 0))
  , defaultTest (assertEqual [ 1 ] (normalize [ 11 ] 10 0))
  , defaultTest (assertEqual [ 8, 2 ] (normalize [ 18, 1 ] 10 0))
  , defaultTest (assertEqual [ 1, 3 ] (normalize [ 18, 1 ] 10 3))
  , defaultTest (assertEqual [ 1, 2, 2 ] (normalize [ 11, 11, 11 ] 10 0))
  , defaultTest (assertEqual [ 2, 2, 2 ] (normalize [ 4, 4, 4 ] 3 1))
  ]

addTest = suite "add"
  [ defaultTest (assertEqual [ 3 ] (add [ 1 ] [ 2 ] 10))
  , defaultTest (assertEqual [ 3 ] (add [ 1 ] [ 2 ] 4))
  , defaultTest (assertEqual [ 3, 6 ] (add [ 1, 5 ] [ 2, 1 ] 10))
  , defaultTest (assertEqual [ 3, 2 ] (add [ 1, 5 ] [ 2, 1 ] 4))
  , defaultTest (assertEqual [ 3, 6 ] (add [ 1, 5 ] [ 2, 1, 2, 2 ] 10))
  , defaultTest (assertEqual [ 3, 2, 2 ] (add [ 1, 5, 1 ] [ 2, 1, 0 ] 4))
  , defaultTest (assertEqual [ 2, 0, 8, 5 ] (add [ 4, 3, 2, 1 ] [ 8, 6, 5, 4 ] 10))
  ]

-----------------
-- conversion routines
-----------------

charToIntTest = suite "charToInt"
  [ defaultTest (assertEqual 0 (charToInt '0'))
  , defaultTest (assertEqual 4 (charToInt '4'))
  , defaultTest (assertEqual 9 (charToInt '9'))
  , defaultTest (assertEqual 10 (charToInt 'a'))
  , defaultTest (assertEqual 28 (charToInt 's'))
  , defaultTest (assertEqual 35 (charToInt 'z'))
  ]

stringToLeftinTest = suite "stringToLeftinTest"
  [ defaultTest (assertEqual [ 1 ] (stringToLeftin "1"))
  , defaultTest (assertEqual [ 3, 11 ] (stringToLeftin "b3"))
  , defaultTest (assertEqual [ 28, 2, 10 ] (stringToLeftin "a2s"))
  ]

intToCharTest = suite "intToChar"
  [ defaultTest (assertEqual '0' (intToChar 0))
  , defaultTest (assertEqual '4' (intToChar 4))
  , defaultTest (assertEqual '9' (intToChar 9))
  , defaultTest (assertEqual 'a' (intToChar 10))
  , defaultTest (assertEqual 's' (intToChar 28))
  , defaultTest (assertEqual 'z' (intToChar 35))
  ]

leftinToStringTest = suite "leftinToString"
  [ defaultTest (assertEqual "1" (leftinToString [ 1 ]))
  , defaultTest (assertEqual "b3" (leftinToString [ 3, 11 ]))
  , defaultTest (assertEqual "a2s" (leftinToString [ 28, 2, 10 ]))
  ]

-----------------
-- overall test suite
-----------------

tests = suite "Leftins Test Suite"
  [ normalizeTest
  , addTest
  , charToIntTest
  , stringToLeftinTest
  , intToCharTest
  , leftinToStringTest
  ]

main =
  runDisplay tests
