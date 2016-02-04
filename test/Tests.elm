module Tests (..) where

import ElmTest exposing (suite, equals, Test)
import IsoSpec
import PrismSpec
import LensSpec


all : Test
all =
    suite
        "A Monocle test suite"
        [ IsoSpec.all
        , PrismSpec.all
        , LensSpec.all
        ]
