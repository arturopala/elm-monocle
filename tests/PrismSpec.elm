module PrismSpec exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, list, int, tuple, string, char)
import String
import Monocle.Iso exposing (Iso)
import Monocle.Prism exposing (Prism)


all : Test
all =
    describe
        "A Prism specification"
        [ test_prism_property_partial_round_trip_one_way
        , test_prism_property_no_round_trip_when_not_matching
        , test_prism_property_round_trip_other_way
        , test_prism_method_modify
        , test_prism_method_modify_option
        , test_prism_method_compose
        , test_prism_method_composeIso
        , test_prism_method_fromIso
        , test_prism_method_fromIso_reverseGet
        ]


string2IntPrism : Prism String Int
string2IntPrism =
    Prism (String.toInt >> Result.toMaybe) toString


string2FloatPrism : Prism String Float
string2FloatPrism =
    Prism (String.toFloat >> Result.toMaybe) toString


float2IntPrism : Prism Float Int
float2IntPrism =
    let
        getOption float =
            let
                int =
                    truncate float

                back =
                    toFloat int
            in
                if (float == back) then
                    Just int
                else
                    Nothing
    in
        Prism getOption toFloat


numbers : Fuzzer String
numbers =
    Fuzz.map toString int


notnumbers : Fuzzer String
notnumbers =
    Fuzz.map (\s -> String.append "_" s) string


numbersAndNotNumbers : Fuzzer String
numbersAndNotNumbers =
    Fuzz.frequency [ ( 0.5, numbers ), ( 0.5, notnumbers ) ]


test_prism_property_partial_round_trip_one_way : Test
test_prism_property_partial_round_trip_one_way =
    let
        prism =
            string2IntPrism

        test s =
            s
                |> prism.getOption
                |> Maybe.map prism.reverseGet
                |> Expect.equal (Just s)
    in
        fuzz numbers "For some a: A, getOption a |> Maybe.map reverseGet == Just a" test


test_prism_property_no_round_trip_when_not_matching : Test
test_prism_property_no_round_trip_when_not_matching =
    let
        prism =
            string2IntPrism

        test s =
            s
                |> prism.getOption
                |> Maybe.map prism.reverseGet
                |> Expect.equal Nothing
    in
        fuzz notnumbers "For some a: A, getOption a |> Maybe.map reverseGet == Nothing " test


test_prism_property_round_trip_other_way : Test
test_prism_property_round_trip_other_way =
    let
        prism =
            string2IntPrism

        test i =
            i
                |> prism.reverseGet
                >> prism.getOption
                |> Expect.equal (Just i)
    in
        fuzz int "For all a: A, getOption (reverseGet a) == Just a " test


test_prism_method_modify : Test
test_prism_method_modify =
    let
        prism =
            string2IntPrism

        computed s =
            let
                fx i =
                    i * 2

                modified =
                    Monocle.Prism.modify prism fx
            in
                modified s

        expected s =
            s |> String.toInt >> Result.toMaybe >> Maybe.map ((*) 2 >> toString) |> Maybe.withDefault s

        test s =
            Expect.equal (computed s) (expected s)
    in
        fuzz string "Prism method modify" test


test_prism_method_modify_option : Test
test_prism_method_modify_option =
    let
        prism =
            string2IntPrism

        computed s =
            let
                fx i =
                    i * 2

                modified =
                    Monocle.Prism.modifyOption prism fx
            in
                modified s

        expected s =
            s |> String.toInt >> Result.toMaybe >> Maybe.map ((*) 2 >> toString)

        test s =
            Expect.equal (computed s) (expected s)
    in
        fuzz numbersAndNotNumbers "Prism method modifyOption" test


test_prism_method_compose : Test
test_prism_method_compose =
    let
        iso =
            Iso ((*) 10) ((//) 10)

        prism =
            Monocle.Prism.composeIso string2IntPrism iso

        computed s =
            prism.getOption s

        expected s =
            s |> String.toInt >> Result.toMaybe >> Maybe.map ((*) 10)

        test s =
            Expect.equal (computed s) (expected s)
    in
        fuzz numbersAndNotNumbers "Prism method compose" test


test_prism_method_composeIso : Test
test_prism_method_composeIso =
    let
        iso =
            Iso ((*) 10) ((//) 10)

        prism =
            Monocle.Prism.composeIso string2IntPrism iso

        computed s =
            prism.getOption s

        expected s =
            s |> String.toInt >> Result.toMaybe >> Maybe.map ((*) 10)

        test s =
            Expect.equal (computed s) (expected s)
    in
        fuzz numbersAndNotNumbers "Prism method composeIso" test


test_prism_method_fromIso : Test
test_prism_method_fromIso =
    let
        iso =
            Iso String.toList String.fromList

        prism =
            Monocle.Prism.fromIso iso

        computed s =
            prism.getOption s

        expected s =
            iso.get s |> Just

        test s =
            Expect.equal (computed s) (expected s)
    in
        fuzz string "Prism method fromIso" test


test_prism_method_fromIso_reverseGet : Test
test_prism_method_fromIso_reverseGet =
    let
        iso =
            Iso String.fromList String.toList

        prism =
            Monocle.Prism.fromIso iso

        computed s =
            prism.reverseGet s

        expected s =
            iso.reverseGet s

        test s =
            Expect.equal (computed s) (expected s)
    in
        fuzz string "Prism method fromIso when reverseGet" test
