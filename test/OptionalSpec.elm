module OptionalSpec (all) where

import ElmTest exposing (suite, equals, Test)
import Check exposing (that, is, for, claim, check)
import Check.Test exposing (test, assert)
import Check.Investigator exposing (Investigator, tuple, string, list, char, int)
import Random exposing (initialSeed)
import Random.Int
import Random.Char
import Random.String
import Random.Extra exposing (constant, merge)
import Shrink
import String
import Char
import Result
import Monocle.Iso exposing (Iso)
import Monocle.Prism exposing (Prism)
import Monocle.Lens exposing (Lens, fromIso)
import Monocle.Optional exposing (Optional, fromPrism, compose, composeLens, modifyOption, modify)
import Maybe exposing (Maybe)


all : Test
all =
    suite
        "An Optional specification"
        [ test_optional_property_identity_when_just
        , test_optional_property_identity_when_nothing
        , test_optional_property_reverse_identity
        , test_optional_method_fromPrism_getOption
        , test_optional_method_fromPrism_set
        , test_optional_method_compose
        , test_optional_method_composeLens
        , test_lens_method_modifyOption_just
        , test_lens_method_modify_just
        , test_lens_method_modifyOption_nothing
        ]


count : Int
count =
    100


seed : Random.Seed
seed =
    initialSeed 25882980


type StreetType
    = Street
    | Avenue


type Country
    = US
    | UK
    | FI
    | PL
    | DE


type alias Address =
    { streetName : String
    , streetType : StreetType
    , floor : Maybe Int
    , town : String
    , region : Maybe String
    , postcode : String
    , country : Country
    }


type alias Place =
    { name : String
    , description : String
    , address : Maybe Address
    }


addressShrinker : Shrink.Shrinker Address
addressShrinker { streetName, streetType, floor, town, region, postcode, country } =
    Address
        `Shrink.map` Shrink.string streetName
        `Shrink.andMap` Shrink.noShrink streetType
        `Shrink.andMap` Shrink.noShrink floor
        `Shrink.andMap` Shrink.string town
        `Shrink.andMap` Shrink.noShrink region
        `Shrink.andMap` Shrink.noShrink postcode
        `Shrink.andMap` Shrink.noShrink country


placeShrinker : Shrink.Shrinker Place
placeShrinker { name, description, address } =
    Place
        `Shrink.map` Shrink.string name
        `Shrink.andMap` Shrink.string description
        `Shrink.andMap` Shrink.noShrink address


addressesWithRegion : Investigator Address
addressesWithRegion =
    let
        address name town postcode region =
            { streetName = name
            , streetType = Street
            , floor = Nothing
            , town = town
            , region = Just region
            , postcode = postcode
            , country = US
            }

        generator = Random.map4 address Random.String.anyEnglishWord Random.String.anyEnglishWord (Random.String.word 5 Random.Char.numberForm) Random.String.anyEnglishWord
    in
        Check.Investigator.investigator generator addressShrinker


addressesWithoutRegion : Investigator Address
addressesWithoutRegion =
    let
        address name town postcode = { streetName = name, streetType = Street, floor = Nothing, town = town, region = Nothing, postcode = postcode, country = US }

        generator = Random.map3 address Random.String.anyEnglishWord Random.String.anyEnglishWord (Random.String.word 5 Random.Char.numberForm)
    in
        Check.Investigator.investigator generator addressShrinker


places : Investigator Place
places =
    let
        addressGenerator = Random.map (\a -> Just a) addressesWithRegion.generator

        generator = Random.map3 Place Random.String.anyEnglishWord Random.String.anyEnglishWord addressGenerator
    in
        Check.Investigator.investigator generator placeShrinker


addressRegionOptional : Optional Address String
addressRegionOptional =
    let
        getOption a = a.region

        set r a = { a | region = Just r }
    in
        Optional getOption set


placeAddressOptional : Optional Place Address
placeAddressOptional =
    let
        getOption p = p.address

        set a p = { p | address = Just a }
    in
        Optional getOption set


string2IntPrism : Prism String Int
string2IntPrism =
    Prism (String.toInt >> Result.toMaybe) toString


string2CharListIso : Iso String (List Char)
string2CharListIso =
    Iso String.toList String.fromList


numbers : Investigator String
numbers =
    Check.Investigator.investigator (Random.Int.intLessThan 10000000 |> Random.map (abs >> toString)) Shrink.string


test_optional_property_identity_when_just =
    let
        opt = addressRegionOptional

        actual a = opt.getOption a |> Maybe.map (\r -> opt.set r a)

        expected a = Just a

        investigator = addressesWithRegion
    in
        test "For some a: A, getOption a |> Maybe.map (r -> set r a)  == Just a" actual expected investigator count seed


test_optional_property_identity_when_nothing =
    let
        opt = addressRegionOptional

        actual a = opt.getOption a |> Maybe.map (\r -> opt.set r a)

        expected a = Nothing

        investigator = addressesWithoutRegion
    in
        test "For some a: A, getOption a |> Maybe.map (r -> set r a)  == Nothing" actual expected investigator count seed


test_optional_property_reverse_identity =
    let
        opt = addressRegionOptional

        actual ( a, r ) = opt.set r a |> opt.getOption

        expected ( _, r ) = Just r

        investigator = Check.Investigator.tuple ( addressesWithoutRegion, string )
    in
        test "For all a: A, set a r |> getOption == Just a" actual expected investigator count seed


test_optional_method_fromPrism_getOption =
    let
        opt = fromPrism string2IntPrism

        actual s = opt.getOption s

        expected s = Just (String.toInt s |> Result.toMaybe |> Maybe.withDefault 0)

        investigator = numbers
    in
        test "Optional.fromPrism.getOption" actual expected investigator count seed


test_optional_method_fromPrism_set =
    let
        opt = fromPrism string2IntPrism

        actual i = opt.set i ""

        expected i = toString i

        investigator = int
    in
        test "Optional.fromPrism.set" actual expected investigator count seed


test_optional_method_compose =
    let
        opt = addressRegionOptional `compose` (fromPrism string2IntPrism)

        actual ( a, i ) = opt.set i a

        expected ( a, i ) = { a | region = Just (toString i) }

        investigator = Check.Investigator.tuple ( addressesWithRegion, int )
    in
        test "Optional.compose" actual expected investigator count seed


test_optional_method_composeLens =
    let
        opt = addressRegionOptional `composeLens` (fromIso string2CharListIso)

        actual ( a, cl ) = opt.set cl a

        expected ( a, cl ) = { a | region = Just (String.fromList cl) }

        investigator = Check.Investigator.tuple ( addressesWithRegion, list char )
    in
        test "Optional.composeLens" actual expected investigator count seed


test_lens_method_modifyOption_just =
    let
        f sn = String.reverse sn

        opt = addressRegionOptional

        actual a = modifyOption opt f a

        expected a = opt.getOption a |> Maybe.map String.reverse |> Maybe.map (\b -> opt.set b a)

        investigator = addressesWithRegion
    in
        test "Optional.modifyOption for Just a" actual expected investigator count seed


test_lens_method_modifyOption_nothing =
    let
        f sn = String.reverse sn

        opt = addressRegionOptional

        actual a = modifyOption opt f a

        expected a = opt.getOption a |> Maybe.map String.reverse |> Maybe.map (\b -> opt.set b a)

        investigator = addressesWithoutRegion
    in
        test "Optional.modifyOption for Nothing" actual expected investigator count seed


test_lens_method_modify_just =
    let
        f sn = String.reverse sn

        opt = addressRegionOptional

        actual a = modify opt f a

        expected a = opt.getOption a |> Maybe.map String.reverse |> Maybe.map (\b -> opt.set b a) |> Maybe.withDefault a

        investigator = addressesWithRegion
    in
        test "Optional.modify for Just a" actual expected investigator count seed


test_lens_method_modify_nothing =
    let
        f sn = String.reverse sn

        opt = addressRegionOptional

        actual a = modify opt f a

        expected a = opt.getOption a |> Maybe.map String.reverse |> Maybe.map (\b -> opt.set b a) |> Maybe.withDefault a

        investigator = addressesWithoutRegion
    in
        test "Optional.modify for Nothing" actual expected investigator count seed
