module Tests exposing (..)

import Expect
import GridTests
import Test exposing (..)


-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!
---- HELPERS ----


all : Test
all =
    describe "Test Suite" [ GridTests.all ]
