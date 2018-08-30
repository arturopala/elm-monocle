module Monocle.Compose.Iso
    exposing
        ( withIso
        , withLens
        , withOptional
        , withPrism
        )

{-| Pipeline-friendly composition helpers from Isos

Using these allow to compose an "outer" optional with an "inner" other optic.

@docs withLens, withOptional, withPrism, withIso

-}

import Monocle.Iso as Iso exposing (Iso)
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import Monocle.Prism exposing (Prism)


{-| pipeline-friendly composition between two Isos

    ab : Iso A B
    ab = ..

    bc : Iso B C
    bc = ..

    cd : Iso C D
    cd = ..

    ad : Iso A D
    ad =
      ab
        |> ComposeIso.withIso bc
        |> ComposeIso.withIso cd

-}
withIso : Iso b c -> Iso a b -> Iso a c
withIso inner outer =
    Iso.compose outer inner


{-| pipeline-friendly composition between an outer Iso and an inner Lens
(the result is a Lens)

    ab : Iso A B
    ab = ..

    bc : Lens B C
    bc = ..

    ad : Lens A D
    ad =
      ab
        |> ComposeIso.withLens bc

-}
withLens : Lens b c -> Iso a b -> Lens a c
withLens inner outer =
    let
        get =
            outer.get >> inner.get

        set c =
            Iso.modify outer (inner.set c)
    in
        Lens get set


{-| pipeline-friendly composition between an outer Iso and an inner Prism
(the result is a Prism)

    ab : Iso A B
    ab = ..

    bc : Prism B C
    bc = ..

    ad : Prism A D
    ad =
      ab
        |> ComposeIso.withPrism bc

-}
withPrism : Prism b c -> Iso a b -> Prism a c
withPrism inner outer =
    let
        getOption =
            outer.get >> inner.getOption

        reverseGet =
            inner.reverseGet >> outer.reverseGet
    in
        Prism getOption reverseGet


{-| pipeline-friendly composition between an outer Iso and an inner Optional
(the result is an Optional)

    ab : Iso A B
    ab = ..

    bc : Optional B C
    bc = ..

    ad : Optional A D
    ad =
      ab
        |> ComposeIso.withOptional bc

-}
withOptional : Optional b c -> Iso a b -> Optional a c
withOptional inner outer =
    let
        getOption =
            outer.get >> inner.getOption

        set c =
            Iso.modify outer (inner.set c)
    in
        Optional getOption set
