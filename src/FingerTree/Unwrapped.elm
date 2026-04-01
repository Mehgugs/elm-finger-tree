module FingerTree.Unwrapped exposing
    ( Tree, Config
    , empty
    , consLeft, consRight, appendList, prependList
    , isEmpty, annotation, head, tail, prefix, last, viewLeft, viewRight, length
    , append, split, splitAt, cut, foldLeft, foldRight, toList, equal
    , Tagged, unconsLeft, unconsRight, cutWithTag, foldLeftWithTag, foldRightWithTag
    )

{-| This module provides an "unwrapped" interface to the finger tree data structure, requiring you to pass the `Config a tag` into the functions explicitly.

In the higher level module there are utilities for switching between the two types (I refer to them as `Tree` and `Unwrapped.Tree`) so that sequences of
operations on unwrapped trees can be expressed tersely.

_This module is an exact mirror of [the FingerTree module](FingerTree); please see the documentation there for information on the functions!_


# Core Types

@docs Tree, Config


# Creation

@docs empty


# Inserting elements

@docs consLeft, consRight, appendList, prependList


# Retrieving elements and subtrees

@docs isEmpty, annotation, head, tail, prefix, last, viewLeft, viewRight, length


# Working on trees

@docs append, split, splitAt, cut, foldLeft, foldRight, toList, equal


# Working with annotations and elements

@docs Tagged, unconsLeft, unconsRight, cutWithTag, foldLeftWithTag, foldRightWithTag

-}

import FingerTree.Internal as Internal
import FingerTree.Types exposing (TreeConfig(..), UnwrappedTree(..))


{-| The Config type records the `tag` type's semantics. This includes the zero value, and the `combine` definition in used.
This type could also be called "annotator" or "decorator", because it tells the functions in this module how they should
annotate the nodes of the tree.
-}
type alias Config a tag =
    TreeConfig a tag


{-| The Tree type is a finger tree with elements of type `a` annotated by type `tag`.
-}
type alias Tree a tag =
    UnwrappedTree a tag


{-| Type alias for pairs of elements and their annotation.
-}
type alias Tagged a tag =
    ( tag, a )


{-| -}
empty : Tree a tag
empty =
    Tree Internal.Empty


{-| -}
toList : Tree a tag -> List a
toList (Tree t) =
    Internal.foldr (\( _, a ) acc -> a :: acc) t []


{-| -}
appendList : Config a tag -> List a -> Tree a tag -> Tree a tag
appendList (Cfg c) items (Tree t) =
    Tree <| List.foldl (\item acc -> Internal.rcons c item acc) t items


{-| -}
prependList : Config a tag -> List a -> Tree a tag -> Tree a tag
prependList (Cfg c) items (Tree t) =
    Tree <| List.foldr (\item acc -> Internal.lcons c item acc) t items


{-| -}
consLeft : Config a tag -> a -> Tree a tag -> Tree a tag
consLeft (Cfg c) a (Tree t) =
    Tree (Internal.lcons c a t)


{-| -}
consRight : Config a tag -> a -> Tree a tag -> Tree a tag
consRight (Cfg c) a (Tree t) =
    Tree (Internal.rcons c a t)


{-| -}
viewLeft : Config a tag -> Tree a tag -> Maybe ( a, Tree a tag )
viewLeft (Cfg c) (Tree t) =
    Internal.viewLeft c t |> Maybe.map (\( ( _, a ), rest ) -> ( a, Tree rest ))


{-| -}
viewRight : Config a tag -> Tree a tag -> Maybe ( a, Tree a tag )
viewRight (Cfg c) (Tree t) =
    Internal.viewRight c t |> Maybe.map (\( ( _, a ), rest ) -> ( a, Tree rest ))


{-| -}
isEmpty : Tree a tag -> Bool
isEmpty (Tree t) =
    case t of
        Internal.Empty ->
            True

        _ ->
            False


{-| -}
head : Tree a tag -> Maybe a
head (Tree t) =
    Internal.head t


{-| -}
tail : Config a tag -> Tree a tag -> Maybe (Tree a tag)
tail (Cfg c) (Tree t) =
    Internal.tail c t |> Maybe.map Tree


{-| -}
prefix : Config a tag -> Tree a tag -> Maybe (Tree a tag)
prefix (Cfg c) (Tree t) =
    Internal.prefix c t |> Maybe.map Tree


{-| -}
last : Config a tag -> Tree a tag -> Maybe a
last (Cfg c) (Tree t) =
    Internal.end c t


{-| -}
append : Config a tag -> Tree a tag -> Tree a tag -> Tree a tag
append (Cfg c) (Tree t1) (Tree t2) =
    Internal.append c t1 t2 |> Tree


{-| -}
split : Config a tag -> (tag -> Bool) -> Tree a tag -> ( Tree a tag, Tree a tag )
split (Cfg c) p (Tree t) =
    let
        ( a, b ) =
            Internal.splitTree c p c.zero t
    in
    ( Tree a, Tree b )


{-| -}
splitAt : Config a comparableTag -> comparableTag -> Tree a comparableTag -> ( Tree a comparableTag, Tree a comparableTag )
splitAt c i tree =
    split c (\j -> j >= i) tree


{-| -}
cut : Config a tag -> (tag -> Bool) -> Tree a tag -> ( Tree a tag, Maybe a, Tree a tag )
cut (Cfg c) p (Tree t) =
    let
        ( l, r ) =
            Internal.splitTree c p c.zero t
    in
    case Internal.viewLeft c r of
        Just ( ( _, x ), rest ) ->
            ( Tree l, Just x, Tree rest )

        _ ->
            ( Tree l, Nothing, Tree r )


{-| -}
foldLeft : (a -> b -> b) -> b -> Tree a tag -> b
foldLeft f init (Tree t) =
    Internal.foldl (\acc ( _, item ) -> f item acc) init t


{-| -}
foldRight : (a -> b -> b) -> b -> Tree a tag -> b
foldRight f init (Tree t) =
    Internal.foldr (\( _, a ) b -> f a b) t init


{-| -}
length : Tree a tag -> Int
length (Tree t) =
    Internal.count t


{-| -}
annotation : Config a tag -> Tree a tag -> tag
annotation (Cfg c) (Tree t) =
    Internal.tagOfTree c t


{-| -}
equal : Config a tag -> Tree a tag -> Tree a tag -> Bool
equal (Cfg c) (Tree t1) (Tree t2) =
    Internal.equal c t1 t2


{-| -}
unconsLeft : Config a tag -> Tree a tag -> Maybe ( Tagged a tag, Tree a tag )
unconsLeft (Cfg c) (Tree t) =
    Internal.viewLeft c t |> Maybe.map (\( a, rest ) -> ( a, Tree rest ))


{-| -}
unconsRight : Config a tag -> Tree a tag -> Maybe ( Tagged a tag, Tree a tag )
unconsRight (Cfg c) (Tree t) =
    Internal.viewRight c t |> Maybe.map (\( a, rest ) -> ( a, Tree rest ))


{-| -}
cutWithTag : Config a tag -> (tag -> Bool) -> Tree a tag -> ( Tree a tag, Maybe (Tagged a tag), Tree a tag )
cutWithTag (Cfg c) p (Tree t) =
    let
        ( l, r ) =
            Internal.splitTree c p c.zero t
    in
    case Internal.viewLeft c r of
        Just ( x, rest ) ->
            ( Tree l, Just x, Tree rest )

        Nothing ->
            ( Tree l, Nothing, Tree r )


{-| -}
foldLeftWithTag : (b -> Tagged a tag -> b) -> b -> Tree a tag -> b
foldLeftWithTag f init (Tree t) =
    Internal.foldl f init t


{-| -}
foldRightWithTag : (Tagged a tag -> b -> b) -> Tree a tag -> b -> b
foldRightWithTag f (Tree t) init =
    Internal.foldr f t init
