module Monocle.Prism (Prism, isMatching, modify, modifyOption, compose, composeIso, fromIso) where

{-| A Prism is a tool which optionally converts elements of type A into elements of type B and back.

# Definition
@docs Prism

# Example

    string2IntPrism : Prism String Int
    string2IntPrism =
        Prism (String.toInt >> Result.toMaybe) toString

    string2IntPrism.getOption "17896" == Just 17896
    string2IntPrism.getOption "1a896" == Nothing
    string2IntPrism.reverseGet 1626767 = "1626767"

# Derived methods
@docs isMatching, modify, modifyOption, compose, composeIso

# Conversion
@docs fromIso

-}

import Maybe
import Monocle.Iso exposing (Iso)


{-| In order to create a `Prism` we need to supply two functions: `getOption` and `reverseGet`
-}
type alias Prism a b =
    { getOption : a -> Maybe b
    , reverseGet : b -> a
    }


{-| Checks if value of type `A` has matching element of type 'B'

        Monocle.Prism.isMatching string2IntPrism "abc" == False
        Monocle.Prism.isMatching string2IntPrism "123" == True
-}
isMatching : Prism a b -> a -> Bool
isMatching prism a =
    case prism.getOption a of
        Just a ->
            True

        Nothing ->
            False


{-| Modifies given function `(b -> b)` to `(a -> Maybe a)` using `Prism a b`

        fx i = i * 2
        modified = Monocle.Prism.modify string2IntPrism fx
        modified "22" == Just "44"
        modified "abc" == Nothing
-}
modifyOption : Prism a b -> (b -> b) -> a -> Maybe a
modifyOption prism f =
    prism.getOption >> Maybe.map (f >> prism.reverseGet)


{-| Modifies given function `(b -> b)` to `(a -> a)` using `Prism a b`

        fx i = i * 2
        modified = Monocle.Prism.modify string2IntPrism fx
        modified "22" == "44"
        modified "abc" == "abc"
-}
modify : Prism a b -> (b -> b) -> a -> a
modify prism f =
    let
        m x = modifyOption prism f x |> Maybe.withDefault x
    in
        m


{-| Composes `Prism a b` with `Prism b c` and returns `Prism a c`

        prism = Monocle.Prism.compose string2FloatPrism float2IntPrism
        prism.getOption "22" == Just 22
        prism.getOption "22.2" == Nothing
        prism.getOption "22a" == Nothing
        prism.getOption "abc" == Nothing

-}
compose : Prism a b -> Prism b c -> Prism a c
compose outer inner =
    let
        getOption x =
            case (outer.getOption x) of
                Just y ->
                    y |> inner.getOption

                Nothing ->
                    Nothing
    in
        Prism (getOption) (inner.reverseGet >> outer.reverseGet)


{-| Composes `Prism a b` with `Iso b c` and returns `Prism a c`

        iso = Iso ((*) 10) ((//) 10)
        prism = Monocle.Prism.composeIso string2IntPrism iso
        prism.getOption "22" == Just 220
        prism.getOption "22.2" == Nothing
        prism.getOption "22a" == Nothing
        prism.getOption "abc" == Nothing
-}
composeIso : Prism a b -> Iso b c -> Prism a c
composeIso outer inner =
    let
        getOption x =
            case (outer.getOption x) of
                Just y ->
                    y |> inner.get |> Just

                Nothing ->
                    Nothing
    in
        Prism (getOption) (inner.reverseGet >> outer.reverseGet)


{-| Casts `Iso a b` to `Prism a b`
-}
fromIso : Iso a b -> Prism a b
fromIso iso =
    Prism (iso.get >> Just) iso.reverseGet
