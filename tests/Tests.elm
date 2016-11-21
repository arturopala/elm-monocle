module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import IsoSpec
import PrismSpec
import LensSpec
import OptionalSpec


all : Test
all =
    describe "Elm Monocle specification"
        [ IsoSpec.all
        , PrismSpec.all
        , LensSpec.all
        , OptionalSpec.all
        ]
