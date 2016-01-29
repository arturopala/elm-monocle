module Monocle.Lens (..) where

{-| Lens is a

    #Definition
    @docs Lens

-}


type alias Lens a b =
    { get : a -> b
    , set : b -> a -> a
    }
