module Monocle.Iso (Iso, reverse, modify, compose) where

{-| An Iso is a tool which converts elements of type A into elements of type B and back without loss.

# Definition
@docs Iso

# Laws
    Identity:  \x -> iso.get(iso.reverseGet x) == x
    Reversed:  \x -> iso.reverseGet(iso.get x) == x

# Example

    string2CharListIso : Iso String (List Char)
    string2CharListIso =
        Iso String.toList String.fromList

    (string2CharListIso.get "ABcdE") == ['A','B','c','d','E']
    (string2CharListIso.reverseGet ['A','B','c','d','E']) == "ABcdE"

# Derived methods
@docs reverse, modify, compose
-}


{-| In order to create an `Iso` we need to supply two total functions: `get` and `reverseGet`
-}
type alias Iso a b =
    { get : a -> b
    , reverseGet : b -> a
    }


{-| Creates reversed `Iso b a`, exchanges functions `get` and `reverseGet`

        .get (Iso.reversed someiso) == someiso.reverseGet
        .reverseGet (Iso.reversed someiso) == someiso.get
        Iso.compose someiso (Iso.reversed someiso) == Iso identity identity
-}
reverse : Iso a b -> Iso b a
reverse iso =
    Iso iso.reverseGet iso.get


{-| Modifies given function `(b -> b)` to be `(a -> a)` using `Iso a b`

        someiso = Iso String.toList String.fromList
        somefx xs =  '@' :: xs
        modified = Iso.modify someiso somefx
        (modified "artur") == "@artur"

-}
modify : Iso a b -> (b -> b) -> a -> a
modify iso f =
    iso.get >> f >> iso.reverseGet


{-| Composes `Iso a b` with `Iso b c` and returns `Iso a c`
-}
compose : Iso a b -> Iso b c -> Iso a c
compose outer inner =
    Iso (outer.get >> inner.get) (inner.reverseGet >> outer.reverseGet)
