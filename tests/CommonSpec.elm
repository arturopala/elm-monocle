module CommonSpec exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string, char)
import String
import Dict
import Maybe
import Array
import Monocle.Common exposing (dict, (=>), (=|>), maybe, array, id)


all : Test
all =
    describe "A Common specification"
        [ test_maybe
        , test_array_just
        , test_array_nothing
        , test_dict_empty
        , test_dict_list
        , test_infix_compose_optionals
        , test_infix_compose_optional_with_lens
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


test_infix_compose_optionals : Test
test_infix_compose_optionals =
    let
        test i =
            .getOption (maybe => array 2) (Just <| Array.fromList [ 10, 11, i, 13 ]) |> Expect.equal (Just i)
    in
        fuzz int "Common.=> should compose 2 Optionals" test


test_infix_compose_optional_with_lens : Test
test_infix_compose_optional_with_lens =
    let
        test i =
            .getOption (maybe =|> id) (Just { id = i }) |> Expect.equal (Just i)
    in
        fuzz int "Common.=|> should compose Optional with Lens" test
