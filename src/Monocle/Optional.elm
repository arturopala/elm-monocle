module Monocle.Optional exposing (Optional, fromPrism, fromLens, compose, composeLens, modifyOption, modify, zip)

{-| A Optional is a weaker Lens and a weaker Prism

# Definition
@docs Optional

# Derived methods
@docs compose, composeLens, modifyOption, modify, zip

# Conversion
@docs fromPrism, fromLens

# Example

    addressRegionOptional : Optional Address String
    addressRegionOptional =
        let
            getOption a = a.region

            set r a = { a | region = Just r }
        in
            Optional getOption set
-}

import Monocle.Prism exposing (Prism)
import Monocle.Lens exposing (Lens)


{-| In order to create Optional we need to suply 2 functions: set and getOption
-}
type alias Optional a b =
    { getOption : a -> Maybe b
    , set : b -> a -> a
    }


{-| Composes `Optional a b` with `Optional b c` and returns `Optional a c`

    string2IntPrism : Prism String Int
    string2IntPrism = Prism (String.toInt >> Result.toMaybe) toString

    addressRegionIntOptional: Optional Address Int
    addressRegionIntOptional = addressRegionOptional `compose` (fromPrism string2IntPrism)
-}
compose : Optional a b -> Optional b c -> Optional a c
compose outer inner =
    let
        set c a =
            outer.getOption a
                |> Maybe.map (\b -> inner.set c b)
                |> Maybe.map (\b -> outer.set b a)
                |> Maybe.withDefault a

        getOption a =
            case (outer.getOption a) of
                Just x ->
                    x |> inner.getOption

                Nothing ->
                    Nothing
    in
        Optional getOption set


{-| Composes `Optional a b` with `Lens b c` and returns `Optional a c`

    string2CharListIso : Iso String (List Char)
    string2CharListIso = Iso String.toList String.fromList

    addressRegionListCharOptional: Optional Address (List Char)
    addressRegionListCharOptional = addressRegionOptional `composeLens` (fromIso string2CharListIso)
-}
composeLens : Optional a b -> Lens b c -> Optional a c
composeLens opt lens =
    let
        set c a =
            opt.getOption a
                |> Maybe.map (\b -> lens.set c b)
                |> Maybe.map (\b -> opt.set b a)
                |> Maybe.withDefault a

        getOption a =
            case (opt.getOption a) of
                Just b ->
                    b |> lens.get |> Just

                Nothing ->
                    Nothing
    in
        Optional getOption set


{-| Modifies given function `(b -> b)` to be `(a -> Maybe a)` using `Optional a b`

        modifyRegion: String -> String
        modifyRegion region = String.reverse region

        modifyAddressRegion: Address -> Maybe Address
        modifyAddressRegion address = Optional.modifyOption addressRegionOptional modifyRegion address
-}
modifyOption : Optional a b -> (b -> b) -> a -> Maybe a
modifyOption opt f =
    let
        mf a = opt.getOption a |> Maybe.map f |> Maybe.map (\b -> opt.set b a)
    in
        mf


{-| Modifies given function `(b -> b)` to be `(a -> a)` using `Optional a b`

        modifyRegion: String -> String
        modifyRegion region = String.reverse region

        modifyAddressRegion: Address -> Address
        modifyAddressRegion address = Optional.modify addressRegionOptional modifyRegion address
-}
modify : Optional a b -> (b -> b) -> a -> a
modify opt f =
    let
        mf a = modifyOption opt f a |> Maybe.withDefault a
    in
        mf


{-| Casts `Prism a b` to `Optional a b`

    string2IntPrism : Prism String Int
    string2IntPrism =
        Prism (String.toInt >> Result.toMaybe) toString

    stringIntOptional: Optional String Int
    stringIntOptional = fromPrism string2IntPrism
-}
fromPrism : Prism a b -> Optional a b
fromPrism prism =
    let
        set b _ = prism.reverseGet b
    in
        Optional prism.getOption set


{-| Casts `Lens a b` to `Optional a b` where `getOption` will return always `Just`
-}
fromLens : Lens a b -> Optional a b
fromLens lens =
    let
        getOption a = Just (lens.get a)
    in
        Optional getOption lens.set


{-| Zip `Optional a c` with `Optional b d` to form Optional for the pairs ( a, b ) ( c, d )
-}
zip : Optional a c -> Optional b d -> Optional ( a, b ) ( c, d )
zip left right =
    let
        getOption ( a, b ) =
            left.getOption a
                |> (\ma ->
                        Maybe.andThen
                            (\c ->
                                right.getOption b
                                    |> Maybe.map (\d -> ( c, d ))
                            )
                            ma
                   )

        set ( c, d ) ( a, b ) =
            ( left.set c a, right.set d b )
    in
        Optional getOption set
