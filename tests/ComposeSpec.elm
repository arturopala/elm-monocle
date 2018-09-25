module ComposeSpec exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (constant, float, int, intRange, oneOf, string, tuple, tuple3)
import Monocle.Iso exposing (Iso)
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import Monocle.Prism exposing (Prism)
import Monocle.Compose as Compose
import Array


all : Test
all =
    describe "Monocle.Compose"
        [ test_isoWithIso
        , test_isoWithLens
        , test_isoWithOptional
        , test_isoWithPrism
        , test_lensWithIso
        , test_lensWithLens
        , test_lensWithOptional
        , test_lensWithPrism
        , test_prismWithIso
        , test_prismWithLens
        , test_prismWithOptional
        , test_prismWithPrism
        , test_optionalWithIso
        , test_optionalWithLens
        , test_optionalWithOptional
        , test_optionalWithPrism
        ]


test_isoWithIso : Test
test_isoWithIso =
    let
        isoString2CharList =
            Iso String.toList String.fromList

        isoCharList2CharArray =
            Iso Array.fromList Array.toList

        isoComposedString2CharArray =
            isoString2CharList
                |> Compose.isoWithIso isoCharList2CharArray

        string2CharArray =
            String.toList >> Array.fromList

        test_get s =
            s
                |> .get isoComposedString2CharArray
                |> Expect.equal (s |> string2CharArray)

        test_reverseGet s =
            (s |> string2CharArray)
                |> .reverseGet isoComposedString2CharArray
                |> Expect.equal s
    in
        describe "Compose.isoWithIso"
            [ test_get |> fuzz string ".get"
            , test_reverseGet |> fuzz string ".reverseGet"
            ]


type Admin name
    = Admin name


type User name
    = User name


test_isoWithLens : Test
test_isoWithLens =
    let
        isoUser2Admin =
            Iso (\(User name) -> Admin name) (\(Admin name) -> User name)

        lensAdminName =
            Lens (\(Admin name) -> name) (\name admin -> Admin name)

        lensComposedUser2AdminName =
            isoUser2Admin
                |> Compose.isoWithLens lensAdminName

        test_get name =
            User name
                |> .get lensComposedUser2AdminName
                |> Expect.equal name

        test_set ( oldName, newName ) =
            User oldName
                |> .set lensComposedUser2AdminName newName
                |> Expect.equal (User newName)
    in
        describe "Compose.isoWithLens"
            [ test_get |> fuzz string ".get"
            , test_set |> fuzz (tuple ( string, string )) ".set"
            ]


test_isoWithOptional : Test
test_isoWithOptional =
    let
        isoUser2Admin =
            Iso (\(User name) -> Admin name) (\(Admin name) -> User name)

        optionalAdminName =
            Optional (\(Admin name) -> name) (\name admin -> Admin (Just name))

        optionalComposedUser2AdminName =
            isoUser2Admin
                |> Compose.isoWithOptional optionalAdminName

        test_getOption name =
            User (Just name)
                |> .getOption optionalComposedUser2AdminName
                |> Expect.equal (Just name)

        test_set ( oldName, newName ) =
            User (Just oldName)
                |> .set optionalComposedUser2AdminName newName
                |> Expect.equal (User (Just newName))
    in
        describe "Compose.isoWithOptional"
            [ test_getOption |> fuzz string ".getOption"
            , test_set |> fuzz (tuple ( string, string )) ".set"
            ]


test_isoWithPrism : Test
test_isoWithPrism =
    let
        isoCharList2String =
            Iso String.fromList String.toList

        prismString2Int =
            Prism String.toInt String.fromInt

        int2CharList =
            String.fromInt >> String.toList

        prismComposedCharList2Int =
            isoCharList2String
                |> Compose.isoWithPrism prismString2Int

        test_getOption int =
            (int |> int2CharList)
                |> .getOption prismComposedCharList2Int
                |> Expect.equal (Just int)

        test_reverseGet int =
            int
                |> .reverseGet prismComposedCharList2Int
                |> Expect.equal (int |> int2CharList)
    in
        describe "Compose.isoWithPrism"
            [ test_getOption |> fuzz int ".getOption"
            , test_reverseGet |> fuzz int ".reverseGet"
            ]


test_lensWithIso : Test
test_lensWithIso =
    let
        lensUser2Name =
            Lens .name (\name user -> { user | name = name })

        isoReverse =
            Iso String.reverse String.reverse

        lensUser2NameReverse =
            lensUser2Name
                |> Compose.lensWithIso isoReverse

        test_get name =
            { name = name }
                |> .get lensUser2NameReverse
                |> Expect.equal (name |> String.reverse)

        test_set ( oldName, newName ) =
            { name = oldName }
                |> .set lensUser2NameReverse newName
                |> Expect.equal { name = newName |> String.reverse }
    in
        describe "Compose.lensWithIso"
            [ test_get |> fuzz string ".get"
            , test_set |> fuzz (tuple ( string, string )) ".set"
            ]


test_lensWithLens : Test
test_lensWithLens =
    let
        lensPerson2StreetAddress =
            Lens .streetAddress (\streetAddress person -> { person | streetAddress = streetAddress })

        lensStreetAdress2City =
            Lens .city (\city streetAddress -> { streetAddress | city = city })

        lensPerson2StreetAddressCity =
            lensPerson2StreetAddress
                |> Compose.lensWithLens lensStreetAdress2City

        personify ( name, street, city ) =
            { name = name
            , streetAddress =
                { street = street
                , city = city
                }
            }

        test_get ( name, street, city ) =
            personify ( name, street, city )
                |> .get lensPerson2StreetAddressCity
                |> Expect.equal city

        test_set ( ( name, street, city ), newCity ) =
            personify ( name, street, city )
                |> .set lensPerson2StreetAddressCity newCity
                |> Expect.equal (personify ( name, street, newCity ))
    in
        describe "Compose.lensWithLens"
            [ test_get |> fuzz (tuple3 ( string, string, string )) ".get"
            , test_set |> fuzz (tuple ( tuple3 ( string, string, string ), string )) ".set"
            ]


test_lensWithOptional : Test
test_lensWithOptional =
    let
        lensDriver2Vehicle =
            Lens .vehicle (\vehicle driver -> { driver | vehicle = vehicle })

        optionalVehicle2LicenseNumber =
            Optional .licenseNumber (\licenseNumber vehicle -> { vehicle | licenseNumber = Just licenseNumber })

        optionalDriver2LicenseNumber =
            lensDriver2Vehicle
                |> Compose.lensWithOptional optionalVehicle2LicenseNumber

        vehiclify ( name, serie, licenseNumber ) =
            { name = name
            , vehicle =
                { serie = serie
                , licenseNumber = Just licenseNumber
                }
            }

        test_getOption ( name, serie, licenseNumber ) =
            vehiclify ( name, serie, licenseNumber )
                |> .getOption optionalDriver2LicenseNumber
                |> Expect.equal (Just licenseNumber)

        test_set ( ( name, serie, licenseNumber ), newLicenseNumber ) =
            vehiclify ( name, serie, licenseNumber )
                |> .set optionalDriver2LicenseNumber newLicenseNumber
                |> Expect.equal (vehiclify ( name, serie, newLicenseNumber ))
    in
        describe "Compose.lensWithOptional"
            [ test_getOption |> fuzz (tuple3 ( string, string, string )) ".getOption"
            , test_set |> fuzz (tuple ( tuple3 ( string, string, string ), string )) ".set"
            ]


test_lensWithPrism : Test
test_lensWithPrism =
    let
        lensPerson2FirstName =
            Lens .firstName (\firstName person -> { person | firstName = firstName })

        prismName2Number =
            Prism String.toInt String.fromInt

        optionalPerson2Number =
            lensPerson2FirstName
                |> Compose.lensWithPrism prismName2Number

        personify ( firstName, lastName ) =
            { firstName = firstName
            , lastName = lastName
            }

        test_getOption ( firstName, lastName ) =
            personify ( firstName |> String.fromInt, lastName )
                |> .getOption optionalPerson2Number
                |> Expect.equal (Just firstName)

        test_set ( ( firstName, lastName ), newFirstName ) =
            personify ( firstName, lastName )
                |> .set optionalPerson2Number newFirstName
                |> Expect.equal (personify ( newFirstName |> String.fromInt, lastName ))
    in
        describe "Compose.lensWithPrism"
            [ test_getOption |> fuzz (tuple ( int, string )) ".getOption"
            , test_set |> fuzz (tuple ( tuple ( string, string ), int )) ".set"
            ]


test_prismWithIso : Test
test_prismWithIso =
    let
        prismString2Int =
            Prism String.toInt String.fromInt

        isoPlusOne =
            Iso (\n -> n + 1) (\n -> n - 1)

        prismIntPlusOne =
            prismString2Int
                |> Compose.prismWithIso isoPlusOne

        test_getOption quantity =
            (quantity |> String.fromInt)
                |> .getOption prismIntPlusOne
                |> Expect.equal (Just (quantity + 1))
    in
        describe "Compose.prismWithIso"
            [ test_getOption |> fuzz int ".getOption" ]


type alias Address =
    { city : String
    , street : Maybe String
    }


type alias Coordinates =
    { lng : Float
    , lat : Float
    }


type Location
    = ByAddress Address
    | ByCoordinates Coordinates


test_prismWithLens : Test
test_prismWithLens =
    let
        coordinates location =
            case location of
                ByCoordinates c ->
                    Just c

                _ ->
                    Nothing

        prismLocationToCoordinates =
            Prism coordinates ByCoordinates

        lensLng =
            Lens .lng (\lng c -> { c | lng = lng })

        optionalLocationLng =
            prismLocationToCoordinates
                |> Compose.prismWithLens lensLng

        test_getOption ( lng, lat ) =
            (ByCoordinates { lng = lng, lat = lat })
                |> .getOption optionalLocationLng
                |> Expect.equal (Just lng)

        test_set ( ( lng, lat ), newLng ) =
            (ByCoordinates { lng = lng, lat = lat })
                |> .set optionalLocationLng newLng
                |> Expect.equal (ByCoordinates { lng = newLng, lat = lat })
    in
        describe "Compose.prismWithLens"
            [ test_getOption |> fuzz (tuple ( float, float )) ".getOption"
            , test_set |> fuzz (tuple ( tuple ( float, float ), float )) ".set"
            ]


test_prismWithOptional : Test
test_prismWithOptional =
    let
        address location =
            case location of
                ByAddress a ->
                    Just a

                _ ->
                    Nothing

        prismLocationToAddress =
            Prism address ByAddress

        optionalStreet =
            Optional .street (\street a -> { a | street = Just street })

        optionalLocationStreet =
            prismLocationToAddress
                |> Compose.prismWithOptional optionalStreet

        test_getOption ( street, city ) =
            (ByAddress { street = Just street, city = city })
                |> .getOption optionalLocationStreet
                |> Expect.equal (Just street)

        test_set ( ( street, city ), newStreet ) =
            (ByAddress { street = Just street, city = city })
                |> .set optionalLocationStreet newStreet
                |> Expect.equal (ByAddress { street = Just newStreet, city = city })
    in
        describe "Compose.prismWithOptional"
            [ test_getOption |> fuzz (tuple ( string, string )) ".getOption"
            , test_set |> fuzz (tuple ( tuple ( string, string ), string )) ".set"
            ]


type Option
    = OptionOne
    | OptionTwo
    | OptionThree


test_prismWithPrism : Test
test_prismWithPrism =
    let
        toOption n =
            case n of
                1 ->
                    Just OptionOne

                2 ->
                    Just OptionTwo

                3 ->
                    Just OptionThree

                _ ->
                    Nothing

        fromOption option =
            case option of
                OptionOne ->
                    1

                OptionTwo ->
                    2

                OptionThree ->
                    3

        anyOption =
            [ OptionOne
            , OptionTwo
            , OptionThree
            ]
                |> List.map constant

        prismString2Int =
            Prism String.toInt String.fromInt

        prismInt2Option =
            Prism toOption fromOption

        prismString2Option =
            prismString2Int
                |> Compose.prismWithPrism prismInt2Option

        test_getOption n =
            (n |> String.fromInt)
                |> .getOption prismString2Option
                |> Expect.equal (n |> toOption)

        test_reverseGet option =
            option
                |> .reverseGet prismString2Option
                |> Expect.equal (option |> fromOption |> String.fromInt)
    in
        describe "Compose.prismWithPrism"
            [ test_getOption |> fuzz (intRange 1 3) ".getOption"
            , test_reverseGet |> fuzz (oneOf anyOption) ".reverseGet"
            ]


test_optionalWithIso : Test
test_optionalWithIso =
    let
        optionalCity =
            Optional .city (\city address -> { address | city = Just city })

        isoReverse =
            Iso String.reverse String.reverse

        optionalCityReverse =
            optionalCity
                |> Compose.optionalWithIso isoReverse

        test_getOption city =
            { city = Just city }
                |> .getOption optionalCityReverse
                |> Expect.equal (Just (city |> String.reverse))

        test_set ( city, newCity ) =
            { city = Just city }
                |> .set optionalCityReverse (newCity |> String.reverse)
                |> Expect.equal ({ city = Just newCity })
    in
        describe "Compose.optionalWithIso"
            [ test_getOption |> fuzz string ".getOption"
            , test_set |> fuzz (tuple ( string, string )) ".set"
            ]


test_optionalWithLens : Test
test_optionalWithLens =
    let
        optionalAddress =
            Optional .address (\address person -> { person | address = Just address })

        lensStreet =
            Lens .street (\street address -> { address | street = street })

        optionalAddressStreet =
            optionalAddress
                |> Compose.optionalWithLens lensStreet

        personify ( name, city, street ) =
            { name = name
            , address =
                Just
                    { city = city
                    , street = street
                    }
            }

        test_getOption ( name, city, street ) =
            personify ( name, city, street )
                |> .getOption optionalAddressStreet
                |> Expect.equal (Just street)

        test_set ( ( name, city, street ), newStreet ) =
            personify ( name, city, street )
                |> .set optionalAddressStreet newStreet
                |> Expect.equal (personify ( name, city, newStreet ))
    in
        describe "Compose.optionalWithLens"
            [ test_getOption |> fuzz (tuple3 ( string, string, string )) ".getOption"
            , test_set |> fuzz (tuple ( tuple3 ( string, string, string ), string )) ".set"
            ]


test_optionalWithOptional : Test
test_optionalWithOptional =
    let
        optionalAddress =
            Optional .address (\address person -> { person | address = Just address })

        optionalStreet =
            Optional .street (\street address -> { address | street = Just street })

        optionalAddressStreet =
            optionalAddress
                |> Compose.optionalWithOptional optionalStreet

        personify ( name, city, street ) =
            { name = name
            , address =
                Just
                    { city = city
                    , street = Just street
                    }
            }

        test_getOption ( name, city, street ) =
            personify ( name, city, street )
                |> .getOption optionalAddressStreet
                |> Expect.equal (Just street)

        test_set ( ( name, city, street ), newStreet ) =
            personify ( name, city, street )
                |> .set optionalAddressStreet newStreet
                |> Expect.equal (personify ( name, city, newStreet ))
    in
        describe "Compose.optionalWithOptional"
            [ test_getOption |> fuzz (tuple3 ( string, string, string )) ".getOption"
            , test_set |> fuzz (tuple ( tuple3 ( string, string, string ), string )) ".set"
            ]


test_optionalWithPrism : Test
test_optionalWithPrism =
    let
        optionalZipCode =
            Optional .zipcode (\zipcode address -> { address | zipcode = Just zipcode })

        prismString2Int =
            Prism String.toInt String.fromInt

        optionalZipCodeString2Int =
            optionalZipCode
                |> Compose.optionalWithPrism prismString2Int

        test_getOption zipcode =
            { zipcode = Just (zipcode |> String.fromInt) }
                |> .getOption optionalZipCodeString2Int
                |> Expect.equal (Just zipcode)

        test_set ( zipcode, newZipCode ) =
            { zipcode = Just (zipcode |> String.fromInt) }
                |> .set optionalZipCodeString2Int newZipCode
                |> Expect.equal ({ zipcode = Just (newZipCode |> String.fromInt) })
    in
        describe "Compose.optionalWithPrism"
            [ test_getOption |> fuzz int ".getOption"
            , test_set |> fuzz (tuple ( int, int )) ".set"
            ]
