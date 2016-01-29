module Tests (..) where

import ElmTest exposing (suite, equals, Test)
import IsoSpec
import PrismSpec


all : Test
all =
    suite
        "A Monocle test suite"
        [ IsoSpec.all
        , PrismSpec.all
        ]
