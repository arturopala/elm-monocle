module Tests exposing (..)

import ElmTest exposing (suite, equals, Test)
import IsoSpec
import PrismSpec
import LensSpec
import OptionalSpec
import OptionalExample

import Element exposing (show)


all : Test
all =
    suite
        "A Monocle test suite"
        [ IsoSpec.all
        , PrismSpec.all
        , LensSpec.all
        , OptionalSpec.all
        ]

main : Element
main = 
    show (stringRunner tests)