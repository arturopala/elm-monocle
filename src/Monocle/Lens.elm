module Monocle.Lens exposing (Lens, fromIso, compose, modify, modify2, modify3, zip, modifyAndMerge, tuple, tuple3)

{-| A Lens is a functional concept which solves a very common problem:
    how to easily update a complex immutable structure,
    for this purpose Lens acts as a zoom into a record.

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
    placeStreetName = compose placeAddressLens addressStreetNameLens

# Derived methods
@docs compose, modify, modify2, modify3, modifyAndMerge, zip, tuple, tuple3

# Conversion
@docs fromIso

-}

import Monocle.Iso exposing (Iso)
import Dict exposing (Dict)


{-| In order to create Lens we need to supply 2 functions: set and get
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
        set c a =
            outer.get a |> inner.set c |> (\b -> outer.set b a)
    in
        Lens (outer.get >> inner.get) set


{-| Modifies given function `(b -> b)` to be `(a -> a)` using `Lens a b`

    addressStreetNameLens = Lens Address String
    fx streetName = String.reverse streeName
    fx2 = Lens.modify addressStreetNameLens fx
    fx2 {streetName="abcdef"} == {streetName="fedcba"}
-}
modify : Lens a b -> (b -> b) -> a -> a
modify lens f =
    let
        mf a =
            lens.get a |> f |> (\b -> lens.set b a)
    in
        mf


{-| Modifies given function `(b,d) -> (b,d)` to be `(a,c) -> (a,c)` using `Lens a b` and `Lens c d`
-}
modify2 : Lens a b -> Lens c d -> (( b, d ) -> ( b, d )) -> ( a, c ) -> ( a, c )
modify2 lens1 lens2 fx =
    let
        mf ( a, c ) =
            ( lens1.get a, lens2.get c ) |> fx |> (\( b, d ) -> ( lens1.set b a, lens2.set d c ))
    in
        mf


{-| Modifies given function `(b,d,f) -> (b,d,f)` to be `(a,c,e) -> (a,c,e)` using `Lens a b` and `Lens c d` and `Lens e f`
-}
modify3 : Lens a b -> Lens c d -> Lens e f -> (( b, d, f ) -> ( b, d, f )) -> ( a, c, e ) -> ( a, c, e )
modify3 lens1 lens2 lens3 fx =
    let
        mf ( a, c, e ) =
            ( lens1.get a, lens2.get c, lens3.get e ) |> fx |> (\( b, d, f ) -> ( lens1.set b a, lens2.set d c, lens3.set f e ))
    in
        mf


{-| Casts `Iso a b` to `Lens a b`
-}
fromIso : Iso a b -> Lens a b
fromIso iso =
    let
        set b _ =
            iso.reverseGet b
    in
        Lens iso.get set


{-| Zips `Lens a c` with `Lens b d` to form Lens ( a, b ) ( c, d )
-}
zip : Lens a c -> Lens b d -> Lens ( a, b ) ( c, d )
zip left right =
    let
        get ( a, b ) =
            ( left.get a, right.get b )

        set ( c, d ) ( a, b ) =
            ( left.set c a, right.set d b )
    in
        Lens get set


{-| Modifies given function `(b -> (b,c))` to be `(a,c) -> (a,c)` using `Lens a b` and `merge` function

-}
modifyAndMerge : Lens a b -> (b -> ( b, c )) -> (c -> c -> c) -> ( a, c ) -> ( a, c )
modifyAndMerge lens fx merge =
    let
        mf ( a, c ) =
            lens.get a
                |> fx
                |> (\( b, c1 ) -> ( lens.set b a, merge c c1 ))
    in
        mf


{-| Tuple `Lens a b` with `Lens a c` and returns `Lens a (b,c)`
-}
tuple : Lens a b -> Lens a c -> Lens a ( b, c )
tuple left right =
    let
        get a =
            ( left.get a, right.get a )

        set ( b, c ) a =
            right.set c (left.set b a)
    in
        Lens get set


{-| Tuple `Lens a b` with `Lens a c` with `Lens a d` and returns `Lens a (b,c,d)`
-}
tuple3 : Lens a b -> Lens a c -> Lens a d -> Lens a ( b, c, d )
tuple3 first second third =
    let
        get a =
            ( first.get a, second.get a, third.get a )

        set ( b, c, d ) a =
            third.set d (second.set c (first.set b a))
    in
        Lens get set
