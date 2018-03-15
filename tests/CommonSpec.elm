module CommonSpec exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string, char)
import String
import Dict
import Maybe
import Monocle.Common exposing (dict)


all : Test
all =
    describe "A Common specification"
        [ test_dict_empty
        , test_dict_list
        ]


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
