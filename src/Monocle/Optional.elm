module Monocle.Optional (Optional) where

{-| A Optional is a weaker Lens and a weaker Prism

# Definition
@docs Optional
-}


{-| In order to create Optional we need to suply 2 functions: set and getOption
-}
type alias Optional a b =
    { getOption : a -> Maybe b
    , set : b -> a -> a
    }
