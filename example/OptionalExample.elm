module OptionalExample (..) where

import Monocle.Optional exposing (Optional)
import Monocle.Lens exposing (Lens)


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
    , description : Maybe String
    , address : Maybe Address
    }


addressOfPlace : Optional Place Address
addressOfPlace =
    let
        getOption p = p.address

        set a p = { p | address = Just a }
    in
        Optional getOption set


regionOfAddress : Optional Address String
regionOfAddress =
    let
        getOption a = a.region

        set r a = { a | region = Just r }
    in
        Optional getOption set


streetNameOfAddress : Lens Address String
streetNameOfAddress =
    let
        get a = a.streetName

        set sn a = { a | streetName = sn }
    in
        Lens get set


regionOfPlace : Optional Place String
regionOfPlace =
    addressOfPlace `Monocle.Optional.compose` regionOfAddress


streetNameOfPlace : Optional Place String
streetNameOfPlace =
    addressOfPlace `Monocle.Optional.composeLens` streetNameOfAddress


place : Place
place =
    { name = "MyPlace"
    , description = Nothing
    , address =
        Just
            { streetName = "Union Road"
            , streetType = Street
            , floor = Nothing
            , town = "Daisytown"
            , region = Nothing
            , postcode = "00100"
            , country = US
            }
    }


updatedPlace : Place
updatedPlace =
    place |> regionOfPlace.set "NorthEast" |> streetNameOfPlace.set "Union Avenue"
