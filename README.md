[![Build Status](https://semaphoreci.com/api/v1/arturopala/elm-monocle/branches/master/badge.svg)](https://semaphoreci.com/arturopala/elm-monocle)

elm-monocle
===========

A [Monocle](http://optics-dev.github.io/Monocle/)-inspired library providing purely functional abstractions to manipulate complex records in the [elm](http://www.elm-lang.org/) language.

Published as [**arturopala/elm-monocle**](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest) library.


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
    Optional .address (\b a -> { a | address = Just b })


regionOfAddress : Optional Address String
regionOfAddress =
    Optional .region (\b a -> { a | region = Just b })


streetNameOfAddress : Lens Address String
streetNameOfAddress =
    Lens .streetName (\b a -> { a | streetName = b })


regionOfPlace : Optional Place String
regionOfPlace =
    addressOfPlace |> Monocle.Compose.optionalWithOptional regionOfAddress


streetNameOfPlace : Optional Place String
streetNameOfPlace =
    addressOfPlace |> Monocle.Compose.optionalWithLens streetNameOfAddress


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
    place
        |> regionOfPlace.set "NorthEast"
        |> streetNameOfPlace.set "Union Avenue"
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
        Prism String.toInt String.fromInt

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
        { streetName: String
        , postcode: String
        , town: String
        }

    type alias Place =
        { name: String
        , address: Address
        }

    addressStreetNameLens : Lens Address String
    addressStreetNameLens =
        Lens .streetName (\b a -> { a | streetName = b })

    placeAddressLens : Lens Place Address
    placeAddressLens =
        Lens .address (\b a -> { a | address = b })

    placeStreetName: Lens Place String
    placeStreetName =
        placeAddressLens |> Monocle.Compose.lensWithLens addressStreetNameLens

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
        Optional .region (\b a -> { a | region = Just b })

    string2IntPrism : Prism String Int
    string2IntPrism = Prism String.toInt String.fromInt

    addressRegionIntOptional: Optional Address Int
    addressRegionIntOptional =
        addressRegionOptional |> Monocle.Compose.optionalWithPrism string2IntPrism

    string2CharListIso : Iso String (List Char)
    string2CharListIso = Iso String.toList String.fromList

    addressRegionListCharOptional: Optional Address (List Char)
    addressRegionListCharOptional =
        addressRegionOptional |> Monocle.Compose.optionalWithIso string2CharListIso

    modifyRegion: String -> String
    modifyRegion region = String.reverse region

    modifyAddressRegion: Address -> Maybe Address
    modifyAddressRegion address = Optional.modifyOption addressRegionOptional modifyRegion address

    modifyRegion: String -> String
    modifyRegion region = String.reverse region

    modifyAddressRegion: Address -> Address
    modifyAddressRegion address = Optional.modify addressRegionOptional modifyRegion address
```

## Traversal

A Traversal allows you to modify many elements at once.

```elm
    type alias Traversal a b =
        (b -> b) -> a -> a
```

(`Traversal a b` is just an alias for a function that applies
a transformation over `b` elements of a larger `a` structure.)

###### Example

```elm
    firstNameLens : Lens Friend String
    firstNameLens =
        Lens .firstName (\b a -> { a | firstName = b })

    bestFriendsTraversal : Traversal (List Friend) Friend
    bestFriendsTraversal =
        Traversal.some
            Traversal.list
            (\friend -> friend.value == Best)

    friendsLens : Lens Account (List Friend)
    friendsLens =
        Lens .friends (\b a -> { a | friends = b })

    firstNamesOfBestFriends : Traversal Account String
    firstNamesOfBestFriends =
        friendsLens
            |> Compose.lensWithTraversal bestFriendsTraversal
            |> Compose.traversalWithLens firstNameLens

    upcaseBestFriendsFirstNames : Account -> Account
    upcaseBestFriendsFirstNames account =
        Traversal.modify firstNamesOfBestFriends String.toUpper
```

## Common
Common lenses/prisms/optionals that most projects will use.

####  Step into a `Maybe` value.
```elm
    maybe.set 5 Nothing
    > Just 5
```
####  Step into an `Array` at the given index.
```elm
    .getOption (array 2) (Array.fromList [ 10, 11, 12, 13 ])
    > Just 12

    .getOption (array 8) (Array.fromList [ 10, 11, 12, 13 ])
    > Nothing
```
####  Step into a `Dict` with the given key.
```elm
    .getOption (dict "Tom") (Dict.fromList [ ( "Tom", "Cat" ) ])
    > Just "Cat"

    .getOption (dict "Jerry") (Dict.fromList [ ( "Tom", "Cat" ) ])
    > Nothing
```
####  Step into the success value of a `Result`.
```elm    
    result.getOption (Ok 5)
    > Just 5

    result.getOption (Err "500")
    > Nothing
```
####  Step into a record with an `id` key.
Since records with an `id` field are incredible common, this is
included for convenience. It also serves as a simple recipe for
creating record lenses.
```elm   
    id.get { id = 1000, name = ... }
    > 1000
```
####  Step into the first element of a pair.
```elm
    first.get ( 'a', 'b' )
    > 'a'
```
####  Step into the second element of a pair.
```elm    
    second.get ( 'a', 'b' )
    > 'b'
```

# Build

## Prerequisites

-   Node.js
-   Yarn
-   Run `yarn install-with-elm`

## Compile

Run `yarn compile`

## Test

Run `elm-test`
