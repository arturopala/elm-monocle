

elm-monocle
===========

A [Monocle](http://julien-truffaut.github.io/Monocle/)-inspired library providing purely functional abstractions to manipulate complex records in the [elm](http://www.elm-lang.org/) language.

Published as [**arturopala/elm-monocle**](http://package.elm-lang.org/packages/arturopala/elm-monocle/1.1.0) library.


# Long Example

```elm
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
```

# Abstractions

## Iso

An Iso is a tool which converts elements of type A into elements of type B and back without loss.

```elm
    type alias Iso a b =
        { get : a -> b
        , reverseGet : b -> a
        }
```

###### Example

```elm
    string2CharListIso : Iso String (List Char)
    string2CharListIso =
        Iso String.toList String.fromList

    (string2CharListIso.get "ABcdE") == ['A','B','c','d','E']
    (string2CharListIso.reverseGet ['A','B','c','d','E']) == "ABcdE"
```

## Prism

A Prism is a tool which optionally converts elements of type A into elements of type B and back.

```elm
    type alias Prism a b =
        { getOption : a -> Maybe b
        , reverseGet : b -> a
        }
```

###### Example

```elm
    string2IntPrism : Prism String Int
    string2IntPrism =
        Prism (String.toInt >> Result.toMaybe) toString

    string2IntPrism.getOption "17896" == Just 17896
    string2IntPrism.getOption "1a896" == Nothing
    string2IntPrism.reverseGet 1626767 = "1626767"
```

## Lens

A Lens is a functional concept which solves a very common problem: how to easily update a complex immutable structure, for this purpose Lens acts as a zoom into a record. 

```elm
    type alias Lens a b =
        { get : a -> b
        , set : b -> a -> a
        }
```

###### Example

```elm

    type alias Address = 
        {streetName: String
        ,postcode: String
        ,town: String}

    type alias Place =
        {name: String
        ,address: Address}

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

    placeStreetName: Lens Place String
    placeStreetName = placeAddressLens `compose` addressStreetNameLens

    myPlace = Place "my" (Address "Elm" "00001" "Daisytown")
    placeStreetName.get myPlace == "Elm"
    
    myNewPlace = placeStreetName.set "Oak" myPlace

    placeStreetName.get myNewPlace == "Oak"
    myNewPlace == Place "my" (Address "Oak" "00001" "Daisytown")

```

## Optional

A Optional is a weaker Lens and a weaker Prism.

```elm
    type alias Optional a b =
        { getOption : a -> Maybe b
        , set : b -> a -> a
        }
```

###### Example

```elm
    addressRegionOptional : Optional Address String
    addressRegionOptional =
        let
            getOption a = a.region

            set r a = { a | region = Just r }
        in
            Optional getOption set

    string2IntPrism : Prism String Int
    string2IntPrism = Prism (String.toInt >> Result.toMaybe) toString

    addressRegionIntOptional: Optional Address Int
    addressRegionIntOptional = addressRegionOptional `compose` (fromPrism string2IntPrism)

    string2CharListIso : Iso String (List Char)
    string2CharListIso = Iso String.toList String.fromList

    addressRegionListCharOptional: Optional Address (List Char)
    addressRegionListCharOptional = addressRegionOptional `composeLens` (fromIso string2CharListIso)

    modifyRegion: String -> String
    modifyRegion region = String.reverse region

    modifyAddressRegion: Address -> Maybe Address
    modifyAddressRegion address = Optional.modifyOption addressRegionOptional modifyRegion address

    modifyRegion: String -> String
    modifyRegion region = String.reverse region

    modifyAddressRegion: Address -> Address
    modifyAddressRegion address = Optional.modify addressRegionOptional modifyRegion address
```


# Build

## Prerequisites

-   Node.js
-   run `npm install`

## Compile

Run

`npm run compile`

or 

`npm run compile-and-watch`

## Test

Run

`npm test`
