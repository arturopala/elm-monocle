

elm-monocle
===========

[Monocle](http://julien-truffaut.github.io/Monocle/)-inspired library providing purely functional abstractions to manipulate records in [elm](http://www.elm-lang.org/) language.

# Abstractions

## Iso

An Iso is a tool which converts elements of type A into elements of type B and back without loss.

```elm
    type alias Iso a b =
        { get : a -> b
        , reverseGet : b -> a
        }
```

## Prism

A Prism is a tool which optionally converts elements of type A into elements of type B and back.

```elm
    type alias Prism a b =
        { getOption : a -> Maybe b
        , reverseGet : b -> a
        }
```

## Lens

A Lens is a functional concept which solves a very common problem: how to update a complex immutable structure. Lens acts as a zoom into record.

```elm
    type alias Lens a b =
        { get : a -> b
        , set : b -> a -> a
        }
```