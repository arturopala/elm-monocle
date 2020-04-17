module Monocle.Traversal exposing
    ( Traversal
    , list, array, some, modify
    )

{-| A Traversal is like an Optional that may modify multiple sub-elements, keeping the overlaying structure as is.


# Definition

@docs Traversal


# Example

    personsAge : Lens Person Int
    personsAge =
        let
            get a =
                a.age

            set b a =
                { a | age = b }
        in
        Lens get set

    jeffs : Traversal (List Person) Person
    jeffs =
        Traversal.some Traversal.list (\p -> p.name == "Jeff")

    jeffsAges : Traversal (List Person) Int
    jeffsAges =
        jeffs
            |> Compose.traversalWithLens personsAge


# Derived methods

@docs list, array, some, modify

-}

import Array exposing (Array)


{-| To create a Traversal, you just need to provide a function
that applies a transformation function to some or all elements
in the parent structure.
-}
type alias Traversal a b =
    (b -> b) -> a -> a


{-| A basic traversal that affects all elements in a list
-}
list : Traversal (List a) a
list =
    List.map


{-| A basic traversal that affects all elements in an array
-}
array : Traversal (Array a) a
array =
    Array.map


{-| A traversal that, given another traversal as base, only affects
those traversed elements that satisfy a condition.

    numbers : Traversal (List number) number
    numbers =
        Traversal.list

    oddNumbers : Traversal (List number) number
    oddNumbers =
        Traversal.some numbers (\number -> remainderBy 2 number == 1)

    evenNumbers : Traversal (List number) number
    evenNumbers =
        Traversal.some numbers (\number -> remainderBy 2 number == 0)

-}
some : Traversal a b -> (b -> Bool) -> Traversal a b
some traversal condition transformation =
    let
        conditionedTransformation value =
            if condition value then
                transformation value

            else
                value
    in
    traversal conditionedTransformation


{-| Modifies all elements traversed by `Traversal a b` using function
`(b -> b)` in structure `a`

    personsHairColor : Lens Person String
    personsHairColor =
        Lens .hairColor (\b a-> { a | hairColor = b })

    pauls : Traversal (List Person) Person
    pauls =
        Traversal.some Traversal.list (\person -> person.firstName == 'Paul' )

    paulsHairColor : Traversal (List Person) String
    paulsHairColor =
        pauls
            |> Compose.traversalWithLens personsHairColor

    lighten : String -> String
    lighten hairColor =
        "light " ++ hairColor

    lightenPauls : List Person -> List Person
    lightenPauls =
        Traversal.modify pauls

    lightenPauls [
        { firstName = "Paul"
        , hairColor = "brown"
        },
        { firstName = "Jake"
        , hairColor = "blond"
        }
    ] == [
        { firstName = "Paul"
        , hairColor = "light brown"
        },
        { firstName = "Jake"
        , hairColor = "blond"
        }
    ]

-}
modify : Traversal a b -> (b -> b) -> a -> a
modify traversal =
    traversal
