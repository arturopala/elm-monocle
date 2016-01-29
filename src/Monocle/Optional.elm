module Monocle.Optional (..) where


type alias Optional a b =
    { getOption : a -> Maybe b
    , set : b -> a -> a
    }
