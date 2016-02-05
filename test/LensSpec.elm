module LensSpec (all) where

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
import Monocle.Lens exposing (Lens, compose, modify)
import Maybe exposing (Maybe)


all : Test
all =
    suite
        "A Lens specification"
        [ test_lens_property_identity
        , test_lens_property_identity_reverse
        , test_lens_method_compose
        , test_lens_method_modify
        ]


count : Int
count =
    100


seed : Random.Seed
seed =
    initialSeed 21882980


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
        `Shrink.andMap` addressShrinker address


addresses : Investigator Address
addresses =
    let
        address name town postcode = { streetName = name, streetType = Street, floor = Nothing, town = town, region = Nothing, postcode = postcode, country = US }

        generator = Random.map3 address Random.String.anyEnglishWord Random.String.anyEnglishWord (Random.String.word 5 Random.Char.numberForm)
    in
        Check.Investigator.investigator generator addressShrinker


places : Investigator Place
places =
    let
        generator = Random.map3 Place Random.String.anyEnglishWord Random.String.anyEnglishWord addresses.generator
    in
        Check.Investigator.investigator generator placeShrinker


addressStreetNameLens : Lens Address String
addressStreetNameLens =
    let
        get a = a.streetName

        set sn a = { a | streetName = sn }
    in
        Lens get set


placeAddressLens : Lens Place Address
placeAddressLens =
    let
        get p = p.address

        set a p = { p | address = a }
    in
        Lens get set


test_lens_property_identity =
    let
        lens = addressStreetNameLens

        actual x = lens.set (lens.get x) x

        expected x = x

        investigator = addresses
    in
        test "For all a: A, (set (get a) a) == a" actual expected investigator count seed


test_lens_property_identity_reverse =
    let
        lens = addressStreetNameLens

        actual ( x, a ) = lens.get (lens.set x a)

        expected ( x, _ ) = x

        investigator = Check.Investigator.tuple ( string, addresses )
    in
        test "For all a: A, get (set a a) == a" actual expected investigator count seed


test_lens_method_compose =
    let
        lens = placeAddressLens `compose` addressStreetNameLens

        actual ( sn, p ) = lens.get (lens.set sn p)

        expected ( sn, _ ) = sn

        investigator = Check.Investigator.tuple ( string, places )
    in
        test "Lens.compose" actual expected investigator count seed


test_lens_method_modify =
    let
        f sn = String.reverse sn

        lens = placeAddressLens `compose` addressStreetNameLens

        actual p = p |> (modify lens f)

        expected p = lens.set (String.reverse (lens.get p)) p

        investigator = places
    in
        test "Lens.modify" actual expected investigator count seed
