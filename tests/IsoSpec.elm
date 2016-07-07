module IsoSpec exposing (all)

import ElmTest exposing (suite, equals, Test)
import Check exposing (that, is, for, claim, check)
import Check.Test exposing (test, assert)
import Check.Producer exposing (Producer, tuple, string, list, char)
import Random exposing (initialSeed)
import Random.Extra exposing (constant)
import Shrink
import String
import Char
import Monocle.Iso exposing (Iso)


all : Test
all =
    suite
        "An Iso specification"
        [ test_iso_property_identity
        , test_iso_property_identity_reversed
        , test_iso_method_reverse
        , test_iso_method_modify
        , test_iso_method_compose
        ]


count : Int
count =
    100


seed : Random.Seed
seed =
    initialSeed 21882981


string2CharListIso : Iso String (List Char)
string2CharListIso =
    Iso String.toList String.fromList


charList2StringIso : Iso (List Char) String
charList2StringIso =
    Iso String.fromList String.toList


test_iso_property_identity =
    let
        iso = string2CharListIso

        actual x = x |> iso.get >> iso.reverseGet

        expected x = x

        investigator = string
    in
        test "For all a: A, reverseGet (get a) == a" actual expected investigator count seed


test_iso_property_identity_reversed =
    let
        iso = charList2StringIso

        actual x = x |> iso.reverseGet >> iso.get

        expected x = x

        investigator = string
    in
        test "For all a: A, get (reverseGet a) == a " actual expected investigator count seed


test_iso_method_reverse =
    let
        iso = Monocle.Iso.reverse string2CharListIso

        actual x = x |> iso.get >> iso.reverseGet

        expected x = x

        investigator = list char
    in
        test "Iso.reverse" actual expected investigator count seed


test_iso_method_modify =
    let
        iso = string2CharListIso

        actual x =
            let
                fx xs = 'x' :: xs

                modified = Monocle.Iso.modify iso fx
            in
                modified x

        expected x = String.cons 'x' x

        investigator = string
    in
        test "Iso.modify" actual expected investigator count seed


test_iso_method_compose =
    let
        iso = Monocle.Iso.compose string2CharListIso charList2StringIso

        actual x = iso.get x

        expected x = x

        investigator = string
    in
        test "Iso.compose" actual expected investigator count seed
