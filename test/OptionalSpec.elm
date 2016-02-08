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
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import Maybe exposing (Maybe)


all : Test
all =
    suite
        "An Optional specification"
        [ test_optional_property_identity_when_just
        , test_optional_property_identity_when_nothing
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


addressesWithRegion : Investigator Address
addressesWithRegion =
    let
        address name town postcode region = { streetName = name, streetType = Street, floor = Nothing, town = town, region = Just region, postcode = postcode, country = US }

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
        generator = Random.map3 Place Random.String.anyEnglishWord Random.String.anyEnglishWord addressesWithRegion.generator
    in
        Check.Investigator.investigator generator placeShrinker


addressRegionOptional : Optional Address String
addressRegionOptional =
    let
        getOption a = a.region

        set r a = { a | region = Just r }
    in
        Optional getOption set


placeAddressLens : Lens Place Address
placeAddressLens =
    let
        get p = p.address

        set a p = { p | address = a }
    in
        Lens get set


test_optional_property_identity_when_just =
    let
        opt = addressRegionOptional

        actual x = opt.getOption x |> Maybe.map (\i -> opt.set i x)

        expected x = Just x

        investigator = addressesWithRegion
    in
        test "For some a: A, getOption a |> Maybe.map (i -> set i x)  == Just a" actual expected investigator count seed


test_optional_property_identity_when_nothing =
    let
        opt = addressRegionOptional

        actual x = opt.getOption x |> Maybe.map (\i -> opt.set i x)

        expected x = Nothing

        investigator = addressesWithoutRegion
    in
        test "For some a: A, getOption a |> Maybe.map (i -> set i x)  == Nothing" actual expected investigator count seed
