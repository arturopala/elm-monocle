module Monocle.Common exposing
    ( maybe
    , array
    , list
    , listToArray
    , dict
    , result
    , id
    , first
    , second
    )

{-| Common lenses/prisms/optionals that most projects will use.

@docs (<|>)
@docs (=>)
@docs (=|>)
@docs maybe
@docs array
@docs list
@docs listToArray
@docs dict
@docs result
@docs id
@docs first
@docs second

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Monocle.Iso exposing (Iso)
import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)


{-| Step into a `Maybe` value.

    maybe.set 5 Nothing
        > Just 5

-}
maybe : Optional (Maybe a) a
maybe =
    { getOption = identity
    , set = always << Just
    }


{-| Step into an `Array` at the given index.

    .getOption (array 2) (Array.fromList [ 10, 11, 12, 13 ])
        > Just 12

    .getOption (array 8) (Array.fromList [ 10, 11, 12, 13 ])
        > Nothing

-}
array : Int -> Optional (Array a) a
array index =
    { getOption = Array.get index
    , set = Array.set index
    }


{-| Step into an `List` at the given index.
(shortcut to avoid converting a `List` to an `Array` ; if it is too slow,
consider using `array` and an `Array` instead of a `List` in your data)

    .getOption (list 2) [ 10, 11, 12, 13 ]
        > Just 12

    .getOption (list 8) [ 10, 11, 12, 13 ]
        > Nothing

-}
list : Int -> Optional (List a) a
list index =
    Optional.compose (Optional.fromLens (Lens.fromIso listToArray)) (array index)


{-| Iso that converts a list to an array.

    .get listToArray [ 1, 2, 3, 4 ]
        == Array.fromList [ 1, 2, 3, 4 ]
        > True

    .reverseGet listToArray (Array.fromList [ 9, 8, 7, 6 ])
        == [ 9, 8, 7, 6 ]
        > True

-}
listToArray : Iso (List a) (Array a)
listToArray =
    Iso Array.fromList Array.toList


{-| Step into a `Dict` with the given key.

    .getOption (dict "Tom") (Dict.fromList [ ( "Tom", "Cat" ) ])
        > Just "Cat"

    .getOption (dict "Jerry") (Dict.fromList [ ( "Tom", "Cat" ) ])
        > Nothing

-}
dict : comparable -> Optional (Dict comparable v) v
dict key =
    { getOption = Dict.get key
    , set = Dict.insert key
    }


{-| Step into the success value of a `Result`.

    result.getOption (Ok 5)
        > Just 5

    result.getOption (Err "500")
        > Nothing

-}
result : Optional (Result e a) a
result =
    { getOption = Result.toMaybe
    , set = always << Ok
    }


{-| Step into a record with an `id` key.

    id.get { id = 1000, name = ... }
    > Just 1000

Since records with an `id` field are incredible common, this is
included for convenience. It also serves as a simple recipe for
creating record lenses.

-}
id : Lens { a | id : b } b
id =
    { get = .id
    , set = \a record -> { record | id = a }
    }


{-| Step into the first element of a pair.

    first.get ( 'a', 'b' )
        > Just 'a'

-}
first : Lens ( a, b ) a
first =
    { get = Tuple.first
    , set = \a ( _, b ) -> ( a, b )
    }


{-| Step into the second element of a pair.

    second.get ( 'a', 'b' )
        > Just 'b'

-}
second : Lens ( a, b ) b
second =
    { get = Tuple.second
    , set = \b ( a, _ ) -> ( a, b )
    }
