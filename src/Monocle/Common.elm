module Monocle.Common exposing (..)

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


{-| Convenient Infix operator for composing lenses.
Allows to chain lens composition for deeply nested structures:

    fromAtoB : Lens A B
    fromAtoB = Lense .b (\a b -> { a | b = b })

    fromBtoC : Lens B C
    fromBtoC = Lense .c (\b c -> { b | c = c })

    fromCtoD : Lens C D
    fromCtoD = Lense .d (\c d -> { c | d = d })

    fromDtoE : Lens D E
    fromDtoE = Lense .e (\d e -> { d | e = e })

    fromAtoE : Lens A E
    fromAtoE = fromAtoB <|> fromBtoC <|> fromCtoD <|> fromDtoE

    a : A
    a = { b: { c: { d: { e: "Whatever we want to get" } } } }

    fromAtoE.get a
    => "Whatever we want to get"

    fromAtoE.set a "What we want to set"
    => { b: { c: { d: { e: "What we want to set" } } } }

-}
(<|>) : Lens a b -> Lens b c -> Lens a c
(<|>) =
    Lens.compose


{-| Convenient infix operator for composing optionals.

    .getOption (maybe => array 2) (Just <| Array.fromList [ 10, 11, 12, 13 ])
    => 12

-}
(=>) : Optional a b -> Optional b c -> Optional a c
(=>) =
    Optional.compose


{-| Convenient infix operator for composing optional with lens.

    .getOption (maybe =|> id) (Just { id = 12 })
    => 12

-}
(=|>) : Optional a b -> Lens b c -> Optional a c
(=|>) a b =
    Optional.compose a (Optional.fromLens b)


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
    Optional.fromLens (Lens.fromIso listToArray) => array index


{-| Iso that converts a list to an array.

    .get listToArray [1, 2, 3, 4] == Array.fromList [1, 2, 3, 4]
    > True

    .reverseGet listToArray (Array.fromList [9, 8, 7, 6]) == [ 9, 8, 7, 6 ]
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
