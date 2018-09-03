module Monocle.Compose
    exposing
        ( isoWithIso
        , isoWithLens
        , isoWithOptional
        , isoWithPrism
        , lensWithIso
        , lensWithLens
        , lensWithOptional
        , lensWithPrism
        , prismWithIso
        , prismWithLens
        , prismWithOptional
        , prismWithPrism
        , optionalWithIso
        , optionalWithLens
        , optionalWithOptional
        , optionalWithPrism
        )

{-| Pipeline-friendly composition helpers

Using these allow to compose an "outer" optic with an "inner" other optic.

Optics in functional programming languages that support typeclasses can be
expressed as functions that compose through the composition operator just like
any other functions ; in Elm (plus typclasses), it would look like this:

    lensAtoB >> lensBtoC >> lensCtoD == lensAtoD
    lensAtoB >> optionalBtoC >> prismCtoD == optionalAtoC

But Elm doesn't support typeclasses, so we're stuck with defining composition
functions that look similar like this:

    import Monocle.Compose as Compose
    lensAtoB
      |> Compose.lensWithLens lensBtoC
      |> Compose.lensWithLens lensCtoD
      == lensAtoD
    lensAtoB
      |> Compose.lensWithOptional optionalBtoC
      |> Compose.optionalWithPrism prismCtoD
      == optionalAtoC

This is arguably more "discoverable" and maybe more readable, if more verbose.


# From an Iso

@docs isoWithIso, isoWithPrism, isoWithLens, isoWithOptional


# From a Prism

@docs prismWithIso, prismWithPrism, prismWithLens, prismWithOptional


# From a Lens

@docs lensWithIso, lensWithPrism, lensWithLens, lensWithOptional


# From an Optional

@docs optionalWithIso, optionalWithPrism, optionalWithLens, optionalWithOptional

-}

import Monocle.Iso as Iso exposing (Iso)
import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)
import Monocle.Prism as Prism exposing (Prism)


{-| pipeline-friendly composition between two Iso

    ab : Iso A B
    ab = ..

    bc : Iso B C
    bc = ..

    ac : Iso A C
    ac =
      ab
        |> ComposeIso.isoWithIso bc

-}
isoWithIso : Iso b c -> Iso a b -> Iso a c
isoWithIso inner outer =
    Iso.compose outer inner


{-| pipeline-friendly composition between an outer Iso and an inner Prism
(the result is a Prism)

    ab : Iso A B
    ab = ..

    bc : Prism B C
    bc = ..

    ac : Prism A C
    ac =
      ab
        |> ComposeIso.isoWithPrism bc

-}
isoWithPrism : Prism b c -> Iso a b -> Prism a c
isoWithPrism inner outer =
    let
        getOption =
            outer.get >> inner.getOption

        reverseGet =
            inner.reverseGet >> outer.reverseGet
    in
        Prism getOption reverseGet


{-| pipeline-friendly composition between an outer Iso and an inner Lens
(the result is a Lens)

    ab : Iso A B
    ab = ..

    bc : Lens B C
    bc = ..

    ac : Lens A C
    ac =
      ab
        |> ComposeIso.isoWithLens bc

-}
isoWithLens : Lens b c -> Iso a b -> Lens a c
isoWithLens inner outer =
    let
        get =
            outer.get >> inner.get

        set c =
            Iso.modify outer (inner.set c)
    in
        Lens get set


{-| pipeline-friendly composition between an outer Iso and an inner Optional
(the result is an Optional)

    ab : Iso A B
    ab = ..

    bc : Optional B C
    bc = ..

    ac : Optional A C
    ac =
      ab
        |> ComposeIso.isoWithOptional bc

-}
isoWithOptional : Optional b c -> Iso a b -> Optional a c
isoWithOptional inner outer =
    let
        getOption =
            outer.get >> inner.getOption

        set c =
            Iso.modify outer (inner.set c)
    in
        Optional getOption set


{-| pipeline-friendly composition between an outer Prism and an inner Iso
(the result is a Prism)

    ab : Prism A B
    ab = ..

    bc : Iso B C
    bc = ..

    ac : Prism A C
    ac =
      ab
        |> ComposePrism.prismWithIso bc

-}
prismWithIso : Iso b c -> Prism a b -> Prism a c
prismWithIso inner outer =
    let
        getOption =
            outer.getOption >> Maybe.map inner.get

        reverseGet =
            inner.reverseGet >> outer.reverseGet
    in
        Prism getOption reverseGet


{-| pipeline-friendly composition between two Prisms

    ab : Prism A B
    ab = ..

    bc : Prism B C
    bc = ..

    cd : Prism C D
    cd = ..

    ac : Prism A C
    ac =
      ab
        |> ComposePrism.prismWithPrism bc

-}
prismWithPrism : Prism b c -> Prism a b -> Prism a c
prismWithPrism inner outer =
    Prism.compose outer inner


{-| pipeline-friendly composition between an outer Prism and an inner Lens
(the result is an Optional)

    ab : Prism A B
    ab = ..

    bc : Lens B C
    bc = ..

    ac : Optional A C
    ac =
      ab
        |> ComposePrism.prismWithLens bc

-}
prismWithLens : Lens b c -> Prism a b -> Optional a c
prismWithLens inner outer =
    let
        getOption =
            outer.getOption >> Maybe.map inner.get

        set c =
            Prism.modify outer (inner.set c)
    in
        Optional getOption set


{-| pipeline-friendly composition between an outer Prism and an inner Optional
(the result is an Optional)

    ab : Prism A B
    ab = ..

    bc : Optional B C
    bc = ..

    ac : Optional A C
    ac =
      ab
        |> ComposePrism.prismWithOptional bc

-}
prismWithOptional : Optional b c -> Prism a b -> Optional a c
prismWithOptional inner outer =
    let
        getOption =
            outer.getOption >> Maybe.andThen inner.getOption

        set c =
            Prism.modify outer (inner.set c)
    in
        Optional getOption set


{-| pipeline-friendly composition between an outer Lens and an inner Iso
(the result is a Lens)

    ab : Lens A B
    ab = ..

    bc : Iso B C
    bc = ..

    ac : Lens A C
    ac =
      ab
        |> Compose.lensWithIso bc

-}
lensWithIso : Iso b c -> Lens a b -> Lens a c
lensWithIso inner outer =
    let
        get =
            outer.get >> inner.get

        set c =
            outer.set (inner.reverseGet c)
    in
        Lens get set


{-| pipeline-friendly composition between an outer Lens and an inner Prism
(the result is an Optional)

    ab : Lens A B
    ab = ..

    bc : Prism B C
    bc = ..

    ac : Optional A C
    ac =
      ab
        |> Compose.lensWithPrism bc

-}
lensWithPrism : Prism b c -> Lens a b -> Optional a c
lensWithPrism inner outer =
    let
        getOption =
            outer.get >> inner.getOption

        set c =
            outer.set (inner.reverseGet c)
    in
        Optional getOption set


{-| pipeline-friendly composition between two Lenses

    ab : Lens A B
    ab = ..

    bc : Lens B C
    bc = ..

    ac : Lens A C
    ac =
      ab
        |> Compose.lensWithLens bc

-}
lensWithLens : Lens b c -> Lens a b -> Lens a c
lensWithLens inner outer =
    Lens.compose outer inner


{-| pipeline-friendly composition between an outer Lens and an inner Optional
(the result is an Optional)

    ab : Lens A B
    ab = ..

    bc : Optional B C
    bc = ..

    ac : Optional A C
    ac =
      ab
        |> Compose.lensWithOptional bc

-}
lensWithOptional : Optional b c -> Lens a b -> Optional a c
lensWithOptional inner outer =
    let
        getOption =
            outer.get >> inner.getOption

        set c =
            Lens.modify outer (inner.set c)
    in
        Optional getOption set


{-| pipeline-friendly composition between an outer Optional and an inner Iso
(the result is an Optional)

    ab : Optional A B
    ab = ..

    bc : Iso B C
    bc = ..

    ac : Optional A C
    ac =
      ab
        |> ComposeOptional.optionalWithIso bc

-}
optionalWithIso : Iso b c -> Optional a b -> Optional a c
optionalWithIso inner outer =
    let
        getOption =
            outer.getOption >> Maybe.map inner.get

        set c =
            outer.set (inner.reverseGet c)
    in
        Optional getOption set


{-| pipeline-friendly composition between an outer Optional and an inner Prism
(the result is an Optional)

    ab : Optional A B
    ab = ..

    bc : Prism B C
    bc = ..

    ac : Optional A C
    ac =
      ab
        |> ComposeOptional.withPrism bc

-}
optionalWithPrism : Prism b c -> Optional a b -> Optional a c
optionalWithPrism inner outer =
    let
        getOption =
            outer.getOption >> Maybe.andThen inner.getOption

        set c =
            outer.set (inner.reverseGet c)
    in
        Optional getOption set


{-| pipeline-friendly composition between an outer Optional and an inner Lens
(the result is an Optional)

    ab : Optional A B
    ab = ..

    bc : Lens B C
    bc = ..

    ac : Optional A C
    ac =
      ab
        |> ComposeOptional.optionalWithLens bc

-}
optionalWithLens : Lens b c -> Optional a b -> Optional a c
optionalWithLens inner outer =
    let
        getOption =
            outer.getOption >> Maybe.map inner.get

        set c =
            Optional.modify outer (inner.set c)
    in
        Optional getOption set


{-| pipeline-friendly composition between two Optionals

    ab : Optional A B
    ab = ..

    bc : Optional B C
    bc = ..

    ac : Optional A C
    ac =
      ab
        |> ComposeOptional.optionalWithOptional bc

-}
optionalWithOptional : Optional b c -> Optional a b -> Optional a c
optionalWithOptional inner outer =
    Optional.compose outer inner
