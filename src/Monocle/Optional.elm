module Monocle.Optional exposing (Optional, fromPrism, fromLens, compose, composeLens, modifyOption, modify, modify2, modify3, zip, tuple, tuple3)

{-| A Optional is a weaker Lens and a weaker Prism


# Definition

@docs Optional


# Derived methods

@docs compose, composeLens, modifyOption, modify, modify2, modify3, zip, tuple, tuple3


# Conversion

@docs fromPrism, fromLens


# Example

    addressRegionOptional : Optional Address String
    addressRegionOptional =
        let
            getOption a =
                a.region

            set r a =
                { a | region = Just r }
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
    string2IntPrism =
        Prism (String.toInt >> Result.toMaybe) toString

    addressRegionIntOptional : Optional Address Int
    addressRegionIntOptional =
        compose addressRegionOptional (fromPrism string2IntPrism)

-}
compose : Optional a b -> Optional b c -> Optional a c
compose outer inner =
    let
        set c a =
            outer.getOption a
                |> Maybe.map (inner.set c >> flip outer.set a)
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
    string2CharListIso =
        Iso String.toList String.fromList

    addressRegionListCharOptional : Optional Address (List Char)
    addressRegionListCharOptional =
        composeLens addressRegionOptional (fromIso string2CharListIso)

-}
composeLens : Optional a b -> Lens b c -> Optional a c
composeLens opt lens =
    let
        set c a =
            opt.getOption a
                |> Maybe.map (lens.set c >> flip opt.set a)
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
modifyOption opt fx =
    let
        mf a =
            opt.getOption a |> Maybe.map (fx >> flip opt.set a)
    in
        mf


{-| Modifies given function `(b -> b)` to be `(a -> a)` using `Optional a b`

        modifyRegion: String -> String
        modifyRegion region = String.reverse region

        modifyAddressRegion: Address -> Address
        modifyAddressRegion address = Optional.modify addressRegionOptional modifyRegion address

-}
modify : Optional a b -> (b -> b) -> a -> a
modify opt fx =
    let
        mf a =
            modifyOption opt fx a |> Maybe.withDefault a
    in
        mf


{-| Modifies given function `(b,d) -> (b,d)` to be `(a,c) -> (a,c)` using `Optional a b` and `Optional c d`

    Function will be invoked ONLY when for ALL arguments `a` and `c` method `Optional.getOption` returns some value.

-}
modify2 : Optional a b -> Optional c d -> (( b, d ) -> ( b, d )) -> ( a, c ) -> ( a, c )
modify2 opt1 opt2 fx =
    let
        mf ( a, c ) =
            case ( opt1.getOption a, opt2.getOption c ) of
                ( Just b, Just d ) ->
                    ( b, d ) |> fx |> (\( b1, d1 ) -> ( opt1.set b1 a, opt2.set d1 c ))

                _ ->
                    ( a, c )
    in
        mf


{-| Modifies given function `( b, d, f ) -> ( b, d, f )` to be `( a, c, e ) -> ( a, c, e )` using `Optional a b` and `Optional c d` and `Optional e f`

    Function will be invoked ONLY when for ALL arguments `a`,`c`,`f` method `Optional.getOption` returns some value.

-}
modify3 : Optional a b -> Optional c d -> Optional e f -> (( b, d, f ) -> ( b, d, f )) -> ( a, c, e ) -> ( a, c, e )
modify3 opt1 opt2 opt3 fx =
    let
        mf ( a, c, e ) =
            case ( opt1.getOption a, opt2.getOption c, opt3.getOption e ) of
                ( Just b, Just d, Just f ) ->
                    ( b, d, f ) |> fx |> (\( b1, d1, f1 ) -> ( opt1.set b1 a, opt2.set d1 c, opt3.set f1 e ))

                _ ->
                    ( a, c, e )
    in
        mf


{-| Casts `Prism a b` to `Optional a b`

    string2IntPrism : Prism String Int
    string2IntPrism =
        Prism (String.toInt >> Result.toMaybe) toString

    stringIntOptional : Optional String Int
    stringIntOptional =
        fromPrism string2IntPrism

-}
fromPrism : Prism a b -> Optional a b
fromPrism prism =
    let
        set b _ =
            prism.reverseGet b
    in
        Optional prism.getOption set


{-| Casts `Lens a b` to `Optional a b` where `getOption` will return always `Just`
-}
fromLens : Lens a b -> Optional a b
fromLens lens =
    let
        getOption a =
            Just (lens.get a)
    in
        Optional getOption lens.set


{-| Zip `Optional a c` with `Optional b d` to form Optional for the pairs ( a, b ) ( c, d )
-}
zip : Optional a c -> Optional b d -> Optional ( a, b ) ( c, d )
zip left right =
    let
        getOption ( a, b ) =
            left.getOption a
                |> Maybe.andThen
                    (\c ->
                        right.getOption b
                            |> Maybe.map (\d -> ( c, d ))
                    )

        set ( c, d ) ( a, b ) =
            ( left.set c a, right.set d b )
    in
        Optional getOption set


{-| Tuple `Optional a b` with `Optional a c` and returns `Optional a (b,c)`

    Method `Optional.getOption` returns pair of values only when both given optionals return some value.

-}
tuple : Optional a b -> Optional a c -> Optional a ( b, c )
tuple left right =
    let
        getOption a =
            case ( left.getOption a, right.getOption a ) of
                ( Just b, Just d ) ->
                    Just ( b, d )

                _ ->
                    Nothing

        set ( b, c ) a =
            right.set c (left.set b a)
    in
        Optional getOption set


{-| Tuple `Optional a b` with `Optional a c` with `Optional a d` and returns `Optional a (b,c,d)`

    Method `Optional.getOption` returns triple of values only when all given optionals return some value.

-}
tuple3 : Optional a b -> Optional a c -> Optional a d -> Optional a ( b, c, d )
tuple3 first second third =
    let
        getOption a =
            case ( first.getOption a, second.getOption a, third.getOption a ) of
                ( Just b, Just d, Just f ) ->
                    Just ( b, d, f )

                _ ->
                    Nothing

        set ( b, c, d ) a =
            first.set b a |> second.set c |> third.set d
    in
        Optional getOption set
