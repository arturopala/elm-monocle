module TraversalSpec exposing (all)

import Array
import Expect
import Fuzz exposing (array, int, list)
import Monocle.Traversal as Traversal
import Test exposing (..)


all : Test
all =
    describe
        "A Traversal specification"
        [ test_list_traversal
        , test_array_traversal
        , test_some_traversal
        ]


test_list_traversal : Test
test_list_traversal =
    let
        test s =
            s
                |> Traversal.modify Traversal.list ((*) 2)
                |> Expect.equal (List.map ((*) 2) s)
    in
    fuzz (list int) "Traversal modify on a list" test


test_array_traversal : Test
test_array_traversal =
    let
        test s =
            s
                |> Traversal.modify Traversal.array ((*) 2)
                |> Expect.equal (Array.map ((*) 2) s)
    in
    fuzz (array int) "Traversal modify on an array" test


test_some_traversal : Test
test_some_traversal =
    let
        odds =
            Traversal.some Traversal.list (\n -> remainderBy 2 n == 1)

        test _ =
            [ 1, 2, 3, 4, 5 ]
                |> Traversal.modify odds ((+) 3)
                |> Expect.equal [ 4, 2, 6, 4, 8 ]
    in
    Test.test "Traversal modify some elements in an array" test
