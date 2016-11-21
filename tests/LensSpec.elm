module LensSpec exposing (all)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, list, int, tuple, string, char)
import String
import Monocle.Iso exposing (Iso)
import Monocle.Prism exposing (Prism)
import Monocle.Lens exposing (Lens, compose, modify, zip, modifyAndMerge)
import Maybe exposing (Maybe)


all : Test
all =
    describe
        "A Lens specification"
        [ test_lens_property_identity
        , test_lens_property_identity_reverse
        , test_lens_method_compose
        , test_lens_method_modify
        , test_lens_method_zip
        , test_lens_method_modifyAndMerge
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


test_lens_property_identity =
    let
        lens =
            addressStreetNameLens

        test address =
            lens.set (lens.get address) address |> Expect.equal address
    in
        fuzz addresses "For all a: A, (set (get a) a) == a" test


test_lens_property_identity_reverse =
    let
        lens =
            addressStreetNameLens

        test ( street, address ) =
            lens.get (lens.set street address) |> Expect.equal street
    in
        fuzz (Fuzz.tuple ( string, addresses )) "For all a: A, get (set a a) == a" test


test_lens_method_compose =
    let
        lens =
            compose placeAddressLens addressStreetNameLens

        test ( street, place ) =
            lens.get (lens.set street place) |> Expect.equal street
    in
        fuzz (Fuzz.tuple ( string, places )) "Lens compose method" test


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
