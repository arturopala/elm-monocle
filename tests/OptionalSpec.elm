module OptionalSpec exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, list, int, tuple, string, char)
import String
import Monocle.Iso exposing (Iso)
import Monocle.Prism exposing (Prism)
import Monocle.Lens exposing (Lens, fromIso)
import Monocle.Optional exposing (Optional, fromPrism, fromLens, compose, composeLens, modifyOption, modify, zip)
import Maybe exposing (Maybe)


all : Test
all =
    describe
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
        , test_optional_method_zip
        , test_optional_method_fromLens
        ]


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


addressRegionOptional : Optional Address String
addressRegionOptional =
    let
        getOption a =
            a.region

        set r a =
            { a | region = Just r }
    in
        Optional getOption set


addressStreetNameLens : Lens Address String
addressStreetNameLens =
    let
        get a =
            a.streetName

        set sn a =
            { a | streetName = sn }
    in
        Lens get set


placeAddressOptional : Optional Place Address
placeAddressOptional =
    let
        getOption p =
            p.address

        set a p =
            { p | address = Just a }
    in
        Optional getOption set


string2IntPrism : Prism String Int
string2IntPrism =
    Prism String.toInt String.fromInt


string2CharListIso : Iso String (List Char)
string2CharListIso =
    Iso String.toList String.fromList


addressesWithRegion : Fuzzer Address
addressesWithRegion =
    let
        address name town postcode region =
            { streetName = name, streetType = Street, floor = Nothing, town = town, region = Just region, postcode = postcode, country = US }
    in
        Fuzz.map4 address string string string string


addressesWithoutRegion : Fuzzer Address
addressesWithoutRegion =
    let
        address name town postcode =
            { streetName = name, streetType = Street, floor = Nothing, town = town, region = Nothing, postcode = postcode, country = US }
    in
        Fuzz.map3 address string string string


places : Fuzzer Place
places =
    Fuzz.map3 Place string string (Fuzz.maybe addressesWithRegion)


numbers : Fuzzer String
numbers =
    Fuzz.map String.fromInt int


test_optional_property_identity_when_just =
    let
        opt =
            addressRegionOptional

        test a =
            opt.getOption a |> Maybe.map (\r -> opt.set r a) |> Expect.equal (Just a)
    in
        fuzz addressesWithRegion "For some a: A, getOption a |> Maybe.map (r -> set r a)  == Just a" test


test_optional_property_identity_when_nothing =
    let
        opt =
            addressRegionOptional

        test a =
            opt.getOption a |> Maybe.map (\r -> opt.set r a) |> Expect.equal Nothing
    in
        fuzz addressesWithoutRegion "For some a: A, getOption a |> Maybe.map (r -> set r a)  == Nothing" test


test_optional_property_reverse_identity =
    let
        opt =
            addressRegionOptional

        test ( a, r ) =
            opt.set r a |> opt.getOption |> Expect.equal (Just r)
    in
        fuzz (Fuzz.tuple ( addressesWithoutRegion, string )) "For all a: A, set a r |> getOption == Just a" test


test_optional_method_fromPrism_getOption =
    let
        opt =
            fromPrism string2IntPrism

        expected s =
            Just (String.toInt s |> Maybe.withDefault 0)

        test s =
            opt.getOption s |> Expect.equal (expected s)
    in
        fuzz numbers "Optional.fromPrism.getOption" test


test_optional_method_fromPrism_set =
    let
        opt =
            fromPrism string2IntPrism

        test i =
            opt.set i "" |> Expect.equal (String.fromInt i)
    in
        fuzz int "Optional.fromPrism.set" test


test_optional_method_compose =
    let
        opt =
            compose addressRegionOptional (fromPrism string2IntPrism)

        computed ( a, i ) =
            opt.set i a

        expected ( a, i ) =
            { a | region = Just (String.fromInt i) }
    in
        fuzz (Fuzz.tuple ( addressesWithRegion, int )) "Optional.compose" (\s -> Expect.equal (computed s) (expected s))


test_optional_method_composeLens =
    let
        opt =
            composeLens addressRegionOptional (fromIso string2CharListIso)

        computed ( a, cl ) =
            opt.set cl a

        expected ( a, cl ) =
            { a | region = Just (String.fromList cl) }
    in
        fuzz (Fuzz.tuple ( addressesWithRegion, list char )) "Optional.composeLens" (\s -> Expect.equal (computed s) (expected s))


test_lens_method_modifyOption_just =
    let
        f sn =
            String.reverse sn

        opt =
            addressRegionOptional

        computed a =
            modifyOption opt f a

        expected a =
            opt.getOption a |> Maybe.map String.reverse |> Maybe.map (\b -> opt.set b a)

        test s =
            Expect.equal (computed s) (expected s)
    in
        fuzz addressesWithRegion "Optional.modifyOption for Just a" test


test_lens_method_modifyOption_nothing =
    let
        f sn =
            String.reverse sn

        opt =
            addressRegionOptional

        computed a =
            modifyOption opt f a

        expected a =
            opt.getOption a |> Maybe.map String.reverse |> Maybe.map (\b -> opt.set b a)

        test s =
            Expect.equal (computed s) (expected s)
    in
        fuzz addressesWithoutRegion "Optional.modifyOption for Nothing" test


test_lens_method_modify_just =
    let
        f sn =
            String.reverse sn

        opt =
            addressRegionOptional

        computed a =
            modify opt f a

        expected a =
            opt.getOption a |> Maybe.map String.reverse |> Maybe.map (\b -> opt.set b a) |> Maybe.withDefault a

        test s =
            Expect.equal (computed s) (expected s)
    in
        fuzz addressesWithRegion "Optional.modify for Just a" test


test_optional_method_zip =
    let
        address1 =
            Address "test" Street Nothing "test" Nothing "test" US

        address2 =
            Address "test" Street Nothing "test" (Just "test") "test" US

        opt =
            zip addressRegionOptional addressRegionOptional

        computed x =
            opt.getOption (opt.set x ( address1, address2 ))

        expected x =
            Just x

        test s =
            Expect.equal (computed s) (expected s)
    in
        fuzz (Fuzz.tuple ( string, string )) "Optional.zip" test


test_optional_method_fromLens =
    let
        opt =
            fromLens addressStreetNameLens

        computed ( a, s ) =
            opt.set s a

        expected ( a, s ) =
            { a | streetName = s }

        test s =
            Expect.equal (computed s) (expected s)
    in
        fuzz (Fuzz.tuple ( addressesWithRegion, string )) "Optional.fromLens" test
