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
  [ defaultTest (assertEqual [ 1 ] (normalize 10 [ 1 ] 0))
  , defaultTest (assertEqual [ 1 ] (normalize 10 [ 11 ] 0))
  , defaultTest (assertEqual [ 8, 2 ] (normalize 10 [ 18, 1 ] 0))
  , defaultTest (assertEqual [ 1, 3 ] (normalize 10 [ 18, 1 ] 3))
  , defaultTest (assertEqual [ 1, 2, 2 ] (normalize 10 [ 11, 11, 11 ] 0))
  , defaultTest (assertEqual [ 2, 2, 2 ] (normalize 3 [ 4, 4, 4 ] 1))
  ]

addTest = suite "add"
  [ defaultTest (assertEqual [ 3 ] (add 10 [ 1 ] [ 2 ]))
  , defaultTest (assertEqual [ 3 ] (add 4 [ 1 ] [ 2 ]))
  , defaultTest (assertEqual [ 3, 6 ] (add 10 [ 1, 5 ] [ 2, 1 ]))
  , defaultTest (assertEqual [ 3, 2 ] (add 4 [ 1, 5 ] [ 2, 1 ]))
  , defaultTest (assertEqual [ 3, 6 ] (add 10 [ 1, 5 ] [ 2, 1, 2, 2 ]))
  , defaultTest (assertEqual [ 3, 2, 2 ] (add 4 [ 1, 5, 1 ] [ 2, 1, 0 ]))
  , defaultTest (assertEqual [ 2, 0, 8, 5 ] (add 10 [ 4, 3, 2, 1 ] [ 8, 6, 5, 4 ]))
  ]

multiplyTest = suite "multiply"
  [ defaultTest (assertEqual [ 2 ] (multiply 10 [ 1 ] [ 2 ]))
  , defaultTest (assertEqual [ 2 ] (multiply 3 [ 1 ] [ 2 ]))
  , defaultTest (assertEqual [ 4 ] (multiply 10 [ 9 ] [ 6 ]))
  , defaultTest (assertEqual [ 2, 2 ] (multiply 3 [ 1, 2 ] [ 2, 1 ]))
  , defaultTest (assertEqual [ 4,8,9,4 ] (multiply 10 [3, 6, 9, 2] [8, 6, 5, 4]))
  , defaultTest (assertEqual [ 4,8,9 ] (multiply 10 [3, 6, 9, 2] [8, 6, 5]))
  -- currently it only properly truncates based on the second argment
  -- , defaultTest (assertEqual [ 4,8,9 ] (multiply 10 [3, 6, 9] [8, 6, 5, 4]))
  ]

powerTest = suite "power"
  [ defaultTest (assertEqual [ 1 ] (power 10 [ 2 ] 0))
  , defaultTest (assertEqual [ 2 ] (power 10 [ 2 ] 1))
  , defaultTest (assertEqual [ 8 ] (power 10 [ 2 ] 3))
  , defaultTest (assertEqual [ 6 ] (power 10 [ 2 ] 4))
  , defaultTest (assertEqual [ 1 ] (power 3 [ 2 ] 2))
  , defaultTest (assertEqual [ 2 ] (power 3 [ 2 ] 3))
  , defaultTest (assertEqual [ 7, 0, 0, 0 ] (power 10 [ 3, 4, 5, 1 ] 3))
  , defaultTest (assertEqual [ 6, 3, 1 ] (power 10 [ 4, 3, 8 ] 4))
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

leftinToIntTest = suite "leftinToInt"
  [ defaultTest (assertEqual 1 (leftinToInt 10 [ 1 ]))
  , defaultTest (assertEqual 179 (leftinToInt 16 [ 3, 11 ]))
  , defaultTest (assertEqual 13060 (leftinToInt 36 [ 28, 2, 10 ]))
  ]

-----------------
-- overall test suite
-----------------

tests = suite "Leftins Test Suite"
  [ normalizeTest
  , addTest
  , multiplyTest
  , powerTest
  , charToIntTest
  , stringToLeftinTest
  , intToCharTest
  , leftinToStringTest
  , leftinToIntTest
  ]

main =
  runDisplay tests
