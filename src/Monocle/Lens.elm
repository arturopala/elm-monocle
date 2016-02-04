module Monocle.Lens (Lens, fromIso) where

{-| A Lens is a functional concept which solves a very common problem:
    how to update a complex immutable structure. Lens acts as a zoom.

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


{-| Casts `Iso a b` to `Lens a b`
-}
fromIso : Iso a b -> Lens a b
fromIso iso =
    let
        set b _ = iso.reverseGet b
    in
        Lens iso.get set
