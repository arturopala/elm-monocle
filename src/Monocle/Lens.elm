module Monocle.Lens (Lens, fromIso, compose) where

{-| A Lens is a functional concept which solves a very common problem:
    how to update a complex immutable structure. Lens acts as a zoom into record.

# Definition
@docs Lens

# Example
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

# Derived methods
@docs compose

# Conversion
@docs fromIso

-}

import Monocle.Iso exposing (Iso)


{-| In order to create Lens we need to suply 2 functions: set and get
-}
type alias Lens a b =
    { get : a -> b
    , set : b -> a -> a
    }


{-| Composes `Lens a b` with `Lens b c` and returns `Lens a c`
-}
compose : Lens a b -> Lens b c -> Lens a c
compose outer inner =
    let
        set c a = outer.get a |> inner.set c |> (\b -> outer.set b a)
    in
        Lens (outer.get >> inner.get) set


{-| Casts `Iso a b` to `Lens a b`
-}
fromIso : Iso a b -> Lens a b
fromIso iso =
    let
        set b _ = iso.reverseGet b
    in
        Lens iso.get set
