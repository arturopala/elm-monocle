module Monocle.Compose.Lens
    exposing
        ( withIso
        , withLens
        , withOptional
        , withPrism
        )

{-| Pipeline-friendly composition helpers from Lenses

Using these allow to compose an "outer" lense with an "inner" other optic.

Optics in functional programming languages that support typeclasses can be
expressed as functions that compose through the composition operator just like
any other functions ; in Elm, that could look like this:

    lensAtoB >> lensBtoC >> lensCtoD == lensAtoD
    lensAtoB >> optionalBtoC >> prismDtoD == optionalBtoC

Elm doesn't support typeclasses, so we're stuck with defining composition
functions that look similar from a module to another just like this:

    import Monocle.Compose.Lens as ComposeLens
    import Monocle.Compose.Optional as ComposeOptional
    lensAtoB
      |> ComposeLens.withLens lensBtoC
      |> ComposeLens.withLens lensCtoD
      == lensAtoD
    lensAtoB
      |> ComposeLens.withOptional optionalBtoC
      |> ComposeOptional.withPrism prismDtoD
      == optionalBtoC

This is arguably more "discoverable" and maybe more readable, if more verbose.

@docs withLens, withOptional, withPrism, withIso

-}

import Monocle.Iso exposing (Iso)
import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import Monocle.Prism exposing (Prism)


{-| pipeline-friendly composition between two Lenses

    ab : Lens A B
    ab = ..

    bc : Lens B C
    bc = ..

    cd : Lens C D
    cd = ..

    ad : Lens A D
    ad =
      ab
        |> ComposeLens.withLens bc
        |> ComposeLens.withLens cd

-}
withLens : Lens b c -> Lens a b -> Lens a c
withLens inner outer =
    Lens.compose outer inner


{-| pipeline-friendly composition between an outer Lens and an inner Optional
(the result is an Optional)

    ab : Lens A B
    ab = ..

    bc : Optional B C
    bc = ..

    ad : Optional A D
    ad =
      ab
        |> ComposeLens.withOptional bc

-}
withOptional : Optional b c -> Lens a b -> Optional a c
withOptional inner outer =
    let
        getOption =
            outer.get >> inner.getOption

        set c =
            Lens.modify outer (inner.set c)
    in
        Optional getOption set


{-| pipeline-friendly composition between an outer Lens and an inner Prism
(the result is an Optional)

    ab : Lens A B
    ab = ..

    bc : Prism B C
    bc = ..

    ad : Optional A D
    ad =
      ab
        |> ComposeLens.withPrism bc

-}
withPrism : Prism b c -> Lens a b -> Optional a c
withPrism inner outer =
    let
        getOption =
            outer.get >> inner.getOption

        set c =
            outer.set (inner.reverseGet c)
    in
        Optional getOption set


{-| pipeline-friendly composition between an outer Lens and an inner Iso
(the result is a Lens)

    ab : Lens A B
    ab = ..

    bc : Iso B C
    bc = ..

    ad : Lens A D
    ad =
      ab
        |> ComposeLens.withIso bc

-}
withIso : Iso b c -> Lens a b -> Lens a c
withIso inner outer =
    let
        get =
            outer.get >> inner.get

        set c =
            outer.set (inner.reverseGet c)
    in
        Lens get set
