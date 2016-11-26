module Monocle.Common exposing (..)

{-| Common lenses/prisms/optionals that most projects will use.

@docs (=>)
@docs maybe
@docs array
@docs dict
@docs result
@docs id
@docs first
@docs second
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)


{-| Convenient infix operator for composing optionals.

   .getOption (maybe => array 2) (Just <| Array.fromList [ 10, 11, 12, 13 ])
   > 12

-}
(=>) : Optional a b -> Optional b c -> Optional a c
(=>) =
    Optional.compose


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
    , set = \id record -> { record | id = id }
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
