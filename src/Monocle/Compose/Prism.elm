module Monocle.Compose.Prism
    exposing
        ( withIso
          -- , withLens
          -- , withOptional
          -- , withPrism
        )

{-| Pipeline-friendly composition helpers from Prisms

Using these allow to compose an "outer" prism with an "inner" other optic.

@docs withLens, withOptional, withPrism, withIso

-}

import Monocle.Iso exposing (Iso)
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import Monocle.Prism as Prism exposing (Prism)


{-| pipeline-friendly composition between two Prisms

    ab : Prism A B
    ab = ..

    bc : Prism B C
    bc = ..

    cd : Prism C D
    cd = ..

    ad : Prism A D
    ad =
      ab
        |> ComposePrism.withPrism bc
        |> ComposePrism.withPrism cd

-}
withPrism : Prism b c -> Prism a b -> Prism a c
withPrism inner outer =
    Prism.compose outer inner


{-| pipeline-friendly composition between an outer Prism and an inner Iso
(the result is a Prism)

    ab : Prism A B
    ab = ..

    bc : Iso B C
    bc = ..

    ad : Prism A D
    ad =
      ab
        |> ComposePrism.withIso bc

-}
withIso : Iso b c -> Prism a b -> Prism a c
withIso inner outer =
    let
        getOption =
            outer.getOption >> Maybe.map inner.get

        set c =
            Prism.modify outer (inner.reverseGet c)
    in
        Prism getOption reverseGet


{-| pipeline-friendly composition between an outer Prism and an inner Lens
(the result is an Optional)

    ab : Prism A B
    ab = ..

    bc : Lens B C
    bc = ..

    ad : Optional A D
    ad =
      ab
        |> ComposePrism.withLens bc

-}
withLens : Lens b c -> Prism a b -> Prism a c
withLens inner outer =
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

    ad : Optional A D
    ad =
      ab
        |> ComposePrism.withOptional bc

-}
withLens : Lens b c -> Prism a b -> Prism a c
withLens inner outer =
    let
        getOption =
            outer.getOption >> Maybe.andThen inner.getOption

        set c =
            Prism.modify outer (inner.set c)
    in
        Optional getOption set
