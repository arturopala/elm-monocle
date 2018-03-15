[![Build Status](https://semaphoreci.com/api/v1/arturopala/elm-monocle/branches/master/badge.svg)](https://semaphoreci.com/arturopala/elm-monocle)

elm-monocle
===========

A [Monocle](http://julien-truffaut.github.io/Monocle/)-inspired library providing purely functional abstractions to manipulate complex records in the [elm](http://www.elm-lang.org/) language.

Published as [**arturopala/elm-monocle**](http://package.elm-lang.org/packages/arturopala/elm-monocle/latest) library.


# Long Example

```elm
import Monocle.Optional exposing (Optional)
import Monocle.Lens exposing (Lens)
import Monocle.Common exposing ((<|>), (=>))


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
    addressOfPlace => regionOfAddress


streetNameOfPlace : Optional Place String
streetNameOfPlace =
    Monocle.Optional.composeLens addressOfPlace streetNameOfAddress


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
    placeStreetName = compose placeAddressLens addressStreetNameLens

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
    addressRegionIntOptional = compose addressRegionOptional (fromPrism string2IntPrism)

    string2CharListIso : Iso String (List Char)
    string2CharListIso = Iso String.toList String.fromList

    addressRegionListCharOptional: Optional Address (List Char)
    addressRegionListCharOptional = composeLens addressRegionOptional (fromIso string2CharListIso)

    modifyRegion: String -> String
    modifyRegion region = String.reverse region

    modifyAddressRegion: Address -> Maybe Address
    modifyAddressRegion address = Optional.modifyOption addressRegionOptional modifyRegion address

    modifyRegion: String -> String
    modifyRegion region = String.reverse region

    modifyAddressRegion: Address -> Address
    modifyAddressRegion address = Optional.modify addressRegionOptional modifyRegion address
```

## Common
Common lenses/prisms/optionals that most projects will use.

#### Convenient infix operator for composing lenses.
Allows to chain lens composition for deeply nested structures:
```elm
    fromAtoB : Lens A B
    fromAtoB = Lens .b (\b a -> { a | b = b })
    fromBtoC : Lens B C
    fromBtoC = Lens .c (\c b -> { b | c = c })
    fromCtoD : Lens C D
    fromCtoD = Lens .d (\d c -> { c | d = d })
    fromDtoE : Lens D E
    fromDtoE = Lens .e (\e d -> { d | e = e })
    fromAtoE : Lens A E
    fromAtoE = fromAtoB <|> fromBtoC <|> fromCtoD <|> fromDtoE

    a : A
    a = { b: { c: { d: { e: "Whatever we want to get" } } } }

    fromAtoE.get a
    => "Whatever we want to get"

    fromAtoE.set a "What we want to set"
    => { b: { c: { d: { e: "What we want to set" } } } }
```
#### Convenient infix operator for composing optionals.
```elm
    .getOption (maybe => array 2) (Just <| Array.fromList [ 10, 11, 12, 13 ])
    > 12
```
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
    > Just 1000
```
####  Step into the first element of a pair.
```elm
    first.get ( 'a', 'b' )
    > Just 'a'
```
####  Step into the second element of a pair.
```elm    
    second.get ( 'a', 'b' )
    > Just 'b'
```

# Build

## Prerequisites

-   Node.js
-   Run `npm run install-with-elm`

## Compile

Run `npm run compile`

## Test

Run `elm-test`
