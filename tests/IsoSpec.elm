module IsoSpec exposing (all)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string, char)
import String
import Monocle.Iso exposing (Iso)


all : Test
all =
    describe "An Iso specification"
        [ test_iso_function_get
        , test_iso_function_reverse_get
        , test_iso_property_identity
        , test_iso_property_identity_reversed
        , test_iso_method_reverse
        , test_iso_method_modify
        , test_iso_method_compose
        ]


string2CharListIso : Iso String (List Char)
string2CharListIso =
    Iso String.toList String.fromList


charList2StringIso : Iso (List Char) String
charList2StringIso =
    Iso String.fromList String.toList


test_iso_function_get : Test
test_iso_function_get =
    let
        iso =
            string2CharListIso

        test s =
            s |> iso.get |> Expect.equal (String.toList s)
    in
        fuzz string "test get function" test


test_iso_function_reverse_get : Test
test_iso_function_reverse_get =
    let
        iso =
            string2CharListIso

        test s =
            (String.toList s) |> iso.reverseGet |> Expect.equal s
    in
        fuzz string "test reverseGet function" test


test_iso_property_identity : Test
test_iso_property_identity =
    let
        iso =
            string2CharListIso

        test s =
            s |> iso.get >> iso.reverseGet |> Expect.equal s
    in
        fuzz string "test identity property: reverseGet(get(x)) == x" test


test_iso_property_identity_reversed : Test
test_iso_property_identity_reversed =
    let
        iso =
            charList2StringIso

        test s =
            s |> iso.reverseGet >> iso.get |> Expect.equal s
    in
        fuzz string "test identity property reversed: get(reverseGet(x)) == x" test


test_iso_method_reverse : Test
test_iso_method_reverse =
    let
        iso =
            string2CharListIso

        isor =
            Monocle.Iso.reverse iso

        test s =
            Expect.equal (iso.get s) (isor.reverseGet s)
    in
        fuzz string "test reverse method" test


test_iso_method_modify : Test
test_iso_method_modify =
    let
        iso =
            string2CharListIso

        test s ch =
            let
                fx xs =
                    ch :: xs

                modified =
                    Monocle.Iso.modify iso fx
            in
                modified s |> Expect.equal (String.cons ch s)
    in
        fuzz2 string char "test modify method" test


test_iso_method_compose : Test
test_iso_method_compose =
    let
        iso =
            Monocle.Iso.compose string2CharListIso charList2StringIso

        test s =
            s |> iso.get |> Expect.equal s
    in
        fuzz string "test compose method" test
