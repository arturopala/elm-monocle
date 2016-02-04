module LensSpec (all) where

import ElmTest exposing (suite, equals, Test)
import Check exposing (that, is, for, claim, check)
import Check.Test exposing (test, assert)
import Check.Investigator exposing (Investigator, tuple, string, list, char, int)
import Random exposing (initialSeed)
import Random.Int
import Random.Extra exposing (constant, merge)
import Shrink
import String
import Char
import Maybe
import Result
import Monocle.Iso exposing (Iso)
import Monocle.Prism exposing (Prism)
import Monocle.Lens exposing (Lens)


all : Test
all =
    suite
        "A Lens specification"
        []


count : Int
count =
    100


seed : Random.Seed
seed =
    initialSeed 21882986
