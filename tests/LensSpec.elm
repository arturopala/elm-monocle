module LensSpec exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, list, int, string, char)
import String
import Monocle.Iso exposing (Iso)
import Monocle.Prism exposing (Prism)
import Monocle.Lens exposing (Lens, compose, modify, modify2, modify3, zip, modifyAndMerge, tuple, tuple3)
import Maybe exposing (Maybe)


all : Test
all =
    describe
        "A Lens specification"
        [ test_lens_property_identity
        , test_lens_property_identity_reverse
        , test_lens_method_compose
        , test_lens_method_modify
        , test_lens_method_modify2
        , test_lens_method_modify3
        , test_lens_method_zip
        , test_lens_method_modifyAndMerge
        , test_lens_method_tuple
        , test_lens_method_tuple3
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
    , address : Address
    }


addressStreetNameLens : Lens Address String
addressStreetNameLens =
    let
        get a =
            a.streetName

        set sn a =
            { a | streetName = sn }
    in
        Lens get set


addressPostcodeLens : Lens Address String
addressPostcodeLens =
    let
        get a =
            a.postcode

        set p a =
            { a | postcode = p }
    in
        Lens get set


addressTownLens : Lens Address String
addressTownLens =
    let
        get a =
            a.town

        set t a =
            { a | town = t }
    in
        Lens get set


placeAddressLens : Lens Place Address
placeAddressLens =
    let
        get p =
            p.address

        set a p =
            { p | address = a }
    in
        Lens get set


addresses : Fuzzer Address
addresses =
    let
        address name town postcode =
            { streetName = name, streetType = Street, floor = Nothing, town = town, region = Nothing, postcode = postcode, country = US }
    in
        Fuzz.map3 address string string string


places : Fuzzer Place
places =
    Fuzz.map3 Place string string addresses


test_lens_property_identity : Test
test_lens_property_identity =
    let
        lens =
            addressStreetNameLens

        test address =
            lens.set (lens.get address) address |> Expect.equal address
    in
        fuzz addresses "For all a: A, (set (get a) a) == a" test


test_lens_property_identity_reverse : Test
test_lens_property_identity_reverse =
    let
        lens =
            addressStreetNameLens

        test ( street, address ) =
            lens.get (lens.set street address) |> Expect.equal street
    in
        fuzz (Fuzz.tuple ( string, addresses )) "For all a: A, get (set a a) == a" test


test_lens_method_compose : Test
test_lens_method_compose =
    let
        lens =
            compose placeAddressLens addressStreetNameLens

        test ( street, place ) =
            lens.get (lens.set street place) |> Expect.equal street
    in
        fuzz (Fuzz.tuple ( string, places )) "Lens compose method" test


test_lens_method_modify : Test
test_lens_method_modify =
    let
        fx street =
            String.reverse street

        lens =
            compose placeAddressLens addressStreetNameLens

        expected place =
            lens.set (String.reverse (lens.get place)) place

        test place =
            place |> (modify lens fx) |> Expect.equal (expected place)
    in
        fuzz places "Lens modify method" test


test_lens_method_modify2 : Test
test_lens_method_modify2 =
    let
        fx ( street, postcode ) =
            ( String.reverse street, String.append "_" postcode )

        lens1 =
            compose placeAddressLens addressStreetNameLens

        lens2 =
            addressPostcodeLens

        lens3 =
            addressPostcodeLens

        expected ( place, address ) =
            ( place |> lens1.set (String.reverse (lens1.get place))
            , address |> lens2.set (String.append "_" (lens2.get address))
            )

        test x =
            x |> (modify2 lens1 lens2 fx) |> Expect.equal (expected x)
    in
        fuzz (Fuzz.tuple ( places, addresses )) "Lens modify2 method" test


test_lens_method_modify3 : Test
test_lens_method_modify3 =
    let
        fx ( street1, street2, street3 ) =
            ( street3, street1, street2 )

        lens =
            compose placeAddressLens addressStreetNameLens

        expected ( place1, place2, place3 ) =
            ( place1 |> lens.set (lens.get place3)
            , place2 |> lens.set (lens.get place1)
            , place3 |> lens.set (lens.get place2)
            )

        test x =
            x |> (modify3 lens lens lens fx) |> Expect.equal (expected x)
    in
        fuzz (Fuzz.tuple3 ( places, places, places )) "Lens modify3 method" test


test_lens_method_zip : Test
test_lens_method_zip =
    let
        address =
            Address "test" Street Nothing "test" Nothing "test" US

        place =
            Place "test" "test" address

        lens =
            zip placeAddressLens addressStreetNameLens

        test x =
            lens.get (lens.set x ( place, address )) |> Expect.equal x
    in
        fuzz (Fuzz.tuple ( addresses, string )) "Lens zip method" test


test_lens_method_modifyAndMerge : Test
test_lens_method_modifyAndMerge =
    let
        lens =
            compose placeAddressLens addressStreetNameLens

        fx a =
            ( String.reverse a, String.length a )

        merge a b =
            a + b

        modifiedFx =
            modifyAndMerge lens fx merge

        expected ( place, n ) =
            ( (lens.set (String.reverse (lens.get place)) place), n + (String.length (lens.get place)) )

        test p =
            modifiedFx p |> Expect.equal (expected p)
    in
        fuzz (Fuzz.tuple ( places, int )) "Lens modifyAndMerge method" test


test_lens_method_tuple : Test
test_lens_method_tuple =
    let
        lens =
            tuple addressStreetNameLens addressPostcodeLens

        test ( address, street, postcode ) =
            lens.get (lens.set ( street, postcode ) address) |> Expect.equal ( street, postcode )
    in
        fuzz (Fuzz.tuple3 ( addresses, string, string )) "Lens tuple method" test


test_lens_method_tuple3 : Test
test_lens_method_tuple3 =
    let
        lens =
            tuple3 addressStreetNameLens addressPostcodeLens addressTownLens

        test ( address, ( street, postcode, town ) ) =
            lens.get (lens.set ( street, postcode, town ) address) |> Expect.equal ( street, postcode, town )
    in
        fuzz (Fuzz.tuple ( addresses, Fuzz.tuple3 ( string, string, string ) )) "Lens tuple3 method" test
