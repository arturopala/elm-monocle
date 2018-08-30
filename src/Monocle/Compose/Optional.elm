module Monocle.Compose.Optional
    exposing
        ( withIso
        , withLens
        , withOptional
        , withPrism
        )

{-| Pipeline-friendly composition helpers from Optionals

Using these allow to compose an "outer" optional with an "inner" other optic.

@docs withLens, withOptional, withPrism, withIso

-}

import Monocle.Iso exposing (Iso)
import Monocle.Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)
import Monocle.Prism exposing (Prism)


{-| pipeline-friendly composition between two Optionals

    ab : Optional A B
    ab = ..

    bc : Optional B C
    bc = ..

    cd : Optional C D
    cd = ..

    ad : Optional A D
    ad =
      ab
        |> ComposeOptional.withOptional bc
        |> ComposeOptional.withOptional cd

-}
withOptional : Optional b c -> Optional a b -> Optional a c
withOptional inner outer =
    Optional.compose outer inner


{-| pipeline-friendly composition between an outer Optional and an inner Lens
(the result is an Optional)

    ab : Optional A B
    ab = ..

    bc : Lens B C
    bc = ..

    ad : Optional A D
    ad =
      ab
        |> ComposeOptional.withLens bc

-}
withLens : Lens b c -> Optional a b -> Optional a c
withLens inner outer =
    let
        getOption =
            outer.getOption >> Maybe.map inner.get

        set c =
            Optional.modify outer (inner.set c)
    in
        Optional getOption set


{-| pipeline-friendly composition between an outer Optional and an inner Prism
(the result is an Optional)

    ab : Optional A B
    ab = ..

    bc : Prism B C
    bc = ..

    ad : Optional A D
    ad =
      ab
        |> ComposeOptional.withPrism bc

-}
withPrism : Prism b c -> Optional a b -> Optional a c
withPrism inner outer =
    let
        getOption =
            outer.getOption >> Maybe.andThen inner.getOption

        set c =
            outer.set (inner.reverseGet c)
    in
        Optional getOption set


{-| pipeline-friendly composition between an outer Optional and an inner Iso
(the result is an Optional)

    ab : Optional A B
    ab = ..

    bc : Iso B C
    bc = ..

    ad : Optional A D
    ad =
      ab
        |> ComposeOptional.withIso bc

-}
withIso : Iso b c -> Optional a b -> Optional a c
withIso inner outer =
    let
        getOption =
            outer.getOption >> Maybe.map inner.get

        set c =
            outer.set (inner.reverseGet c)
    in
        Optional getOption set
