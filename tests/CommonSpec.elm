module CommonSpec exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (int, tuple, string, char)
import Dict
import Maybe
import Array
import Monocle.Common exposing (dict, maybe, array, list, listToArray, id)


all : Test
all =
    describe "A Common specification"
        [ test_maybe
        , test_array_just
        , test_array_nothing
        , test_list_just
        , test_list_nothing
        , test_list_to_array_get
        , test_list_to_array_reverse_get
        , test_dict_empty
        , test_dict_list
        ]


test_maybe : Test
test_maybe =
    let
        test s =
            maybe.set s Nothing |> Expect.equal (Just s)
    in
        fuzz string "Common.maybe should step into Maybe" test


test_array_just : Test
test_array_just =
    let
        test i =
            .getOption (array 2) (Array.fromList [ 10, 11, i, 13 ]) |> Expect.equal (Just i)
    in
        fuzz int "Common.array should get some value at position 2" test


test_array_nothing : Test
test_array_nothing =
    let
        test i =
            .getOption (array 8) (Array.fromList [ i, i, i, i, i, i, i, i ]) |> Expect.equal Nothing
    in
        fuzz int "Common.array should return nothing if index out of bound" test


test_list_just : Test
test_list_just =
    let
        test i =
            .getOption (list 2) [ 10, 11, i, 13 ] |> Expect.equal (Just i)
    in
        fuzz int "Common.list should get some value at position 2" test


test_list_nothing : Test
test_list_nothing =
    let
        test i =
            .getOption (list 8) [ i, i, i, i, i, i, i, i ] |> Expect.equal Nothing
    in
        fuzz int "Common.list should return nothing if index out of bound" test


test_list_to_array_get : Test
test_list_to_array_get =
    let
        test l =
            .get listToArray l |> Expect.equal (l |> Array.fromList)
    in
        fuzz (Fuzz.list int) "Common.listToArray.get should convert a list to an array" test


test_list_to_array_reverse_get : Test
test_list_to_array_reverse_get =
    let
        test a =
            .reverseGet listToArray a |> Expect.equal (Array.toList a)
    in
        fuzz (Fuzz.array int) "Common.listToArray.reverseGet should convert an array to a list" test


test_dict_empty : Test
test_dict_empty =
    let
        opt =
            dict "mykey"

        test s =
            opt.getOption (opt.set s Dict.empty) |> Expect.equal (Just s)
    in
        fuzz string "Common.dict should set and get value by key (empty dict)" test


test_dict_list : Test
test_dict_list =
    let
        opt =
            (dict "Tom")

        test s =
            .getOption opt (Dict.fromList [ ( "Tom", s ), ( "Alice", "Rabbit" ) ]) |> Expect.equal (Just s)
    in
        fuzz string "Common.dict should set and get value by key (preloaded dict)" test
