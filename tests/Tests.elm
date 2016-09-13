module Tests exposing (..)

import Legacy.ElmTest exposing (suite, equals, Test)
import IsoSpec
import PrismSpec
import LensSpec
import OptionalSpec
import OptionalExample

all : Test
all =
    suite
        "A Monocle test suite"
        [ IsoSpec.all
        , PrismSpec.all
        , LensSpec.all
        , OptionalSpec.all
        ]