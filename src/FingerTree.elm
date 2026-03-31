module FingerTree exposing
    ( Config, Tree
    , create, empty
    , consLeft, consRight, appendList, prependList
    , isEmpty, annotation, head, tail, prefix, last, viewLeft, viewRight, length
    , append, split, splitAt, cut, foldLeft, foldRight, toList, equal
    , Tagged, unconsLeft, unconsRight, cutWithTag, foldLeftWithTag, foldRightWithTag
    , config, mempty, mappend, annotator
    )

{-| This package provides an implementation of 2-3 finger trees. Finger trees are a general purpose purely functional data structure that
can be used to efficiently implement many other kinds of data structures like priority queues. A finger tree is an annotated sequence, where each node
stores the result of applying an associative operation to its descendents. This information (the "annotation" or "tag") can be used in a variety of ways to create
new data structures beyond just trees.


# Core Types

@docs Config, Tree


## Note on annotations

To facilitate this data structure the annotations used should form a monoid in respect of the `Config` of the tree.
This means that the `combine` function you provide should be associative, and the `empty` value you provide should act as an identity element: `combine x empty == x` for any annotation `x`.

I have used the term config because elm doesn't have the ability to prove the annotation type chosen does actually form a
monoid under the action of a particular function, and I think for the purposes of defining things like priority queue semantics (one of the main applications of a finger tree)
"config" makes more sense.


# Creation

@docs create, empty


# Inserting elements

@docs consLeft, consRight, appendList, prependList


# Retrieving elements and subtrees

@docs isEmpty, annotation, head, tail, prefix, last, viewLeft, viewRight, length


# Working on trees

@docs append, split, splitAt, cut, foldLeft, foldRight, toList, equal


# Working with annotations and elements

@docs Tagged, unconsLeft, unconsRight, cutWithTag, foldLeftWithTag, foldRightWithTag


# Helpers for working with tree configuration

@docs config, mempty, mappend, annotator

-}

import FingerTree.Internal as Internal


{-| The Config type records the `tag` type's semantics. This includes the zero value, and the `combine` definition in used.
This type could also be called "annotator" or "decorator", because it tells the functions in this module how they should
annotate the nodes of the tree.
-}
type Config a tag
    = Cfg (Internal.Config a tag)


{-| The Tree type is a finger tree with elements of type `a` annotated by type `tag`, instantiated with a particular tag `Config`.
-}
type Tree a tag
    = Tree (Internal.Config a tag) (Internal.Tree a tag)


{-| Type alias for pairs of elements and their tag.
-}
type alias Tagged a tag =
    ( tag, a )


{-| Create a new tree config. You need to provide a function that annotates nodes, an **associative** function which can be used to combine two annotations, and a zero value for the annotations to start with.
The zero value is only used to initialise accumulators, and ideally should not be used as an actual annotation. It should be the identity element of `combine`.
-}
create : { annotate : a -> tag, combine : tag -> tag -> tag, empty : tag } -> Config a tag
create props =
    Cfg { zero = props.empty, combine = props.combine, tag = props.annotate }


{-| The empty value used by the tree config.
-}
mempty : Config a tag -> tag
mempty (Cfg { zero }) =
    zero


{-| The associative operation that combines annotations used by the tree config.
-}
mappend : Config a tag -> (tag -> tag -> tag)
mappend (Cfg { combine }) =
    combine


{-| The function that assigns elements their annotation used by the tree config.
-}
annotator : Config a tag -> (a -> tag)
annotator (Cfg { tag }) =
    tag


{-| Extract the config being used by the tree.
-}
config : Tree a tag -> Config a tag
config (Tree c _) =
    Cfg c


{-| Given the config you want to use, create an empty tree using that config.
-}
empty : Config a tag -> Tree a tag
empty (Cfg c) =
    Tree c Internal.Empty


{-| Convert a tree into a regular elm list. The elements are ordered from left to right.
-}
toList : Tree a tag -> List a
toList (Tree _ t) =
    Internal.foldr (\( _, a ) acc -> a :: acc) t []


{-| Add a list of elements to a tree by consing them to the right side/the end.
-}
appendList : List a -> Tree a tag -> Tree a tag
appendList items (Tree c t) =
    Tree c <| List.foldl (\item acc -> Internal.rcons c item acc) t items


{-| Add a list of elements to a tree by consing them to the left side/the start.
-}
prependList : List a -> Tree a tag -> Tree a tag
prependList items (Tree c t) =
    Tree c <| List.foldr (\item acc -> Internal.lcons c item acc) t items


{-| Add an element to a tree at the left side/the start.
-}
consLeft : a -> Tree a tag -> Tree a tag
consLeft a (Tree c t) =
    Tree c (Internal.lcons c a t)


{-| Add an element to the tree at the right side/the end.
-}
consRight : a -> Tree a tag -> Tree a tag
consRight a (Tree c t) =
    Tree c (Internal.rcons c a t)


{-| Extract the leftmost element from the tree and return it and the remaining tree.
-}
viewLeft : Tree a tag -> Maybe ( a, Tree a tag )
viewLeft (Tree c t) =
    Internal.viewLeft c t |> Maybe.map (\( ( _, a ), rest ) -> ( a, Tree c rest ))


{-| Extract the rightmost element from the tree and return it and the remaining tree.
-}
viewRight : Tree a tag -> Maybe ( a, Tree a tag )
viewRight (Tree c t) =
    Internal.viewRight c t |> Maybe.map (\( ( _, a ), rest ) -> ( a, Tree c rest ))


{-| Check if the tree is empty.
-}
isEmpty : Tree a tag -> Bool
isEmpty (Tree _ t) =
    t == Internal.Empty


{-| Extract the leftmost / first element of the tree.
-}
head : Tree a tag -> Maybe a
head (Tree _ t) =
    Internal.head t


{-| Remove the leftmost / first element of the tree.
-}
tail : Tree a tag -> Maybe (Tree a tag)
tail (Tree c t) =
    Internal.tail c t |> Maybe.map (Tree c)


{-| Remove the rightmost / last element of the tree.
-}
prefix : Tree a tag -> Maybe (Tree a tag)
prefix (Tree c t) =
    Internal.prefix c t |> Maybe.map (Tree c)


{-| Extract the rightmost/last element of the tree.
-}
last : Tree a tag -> Maybe a
last (Tree c t) =
    Internal.end c t


{-| Append two trees, this is like joining the second tree to the end of the first tree.
-}
append : Tree a tag -> Tree a tag -> Tree a tag
append (Tree c t1) (Tree _ t2) =
    Internal.append c t1 t2 |> Tree c


{-| Split a tree using a monotonic predicate on the tags. The first tree of the result is everything before the predicate was true,
and the final result is the everything afterwards.

The predicate being monotonic means that if it's true for a tag `t` then it's also true for any `combine t x`.

-}
split : (tag -> Bool) -> Tree a tag -> ( Tree a tag, Tree a tag )
split p (Tree c t) =
    let
        ( a, b ) =
            Internal.splitTree c p c.zero t
    in
    ( Tree c a, Tree c b )


{-| Split a tree at a the given tag. This function needs the tags to be comparable so that it can split the tree by `tag >= i`.
-}
splitAt : comparableTag -> Tree a comparableTag -> ( Tree a comparableTag, Tree a comparableTag )
splitAt i tree =
    split (\j -> j >= i) tree


{-| Cut the tree, extracting everything before the predicate, the first value for which the predicate is true (if it is) and everything afterward.
-}
cut : (tag -> Bool) -> Tree a tag -> ( Tree a tag, Maybe a, Tree a tag )
cut p (Tree c t) =
    let
        ( l, r ) =
            Internal.splitTree c p c.zero t
    in
    case Internal.viewLeft c r of
        Just ( ( _, x ), rest ) ->
            ( Tree c l, Just x, Tree c rest )

        _ ->
            ( Tree c l, Nothing, Tree c r )


{-| Perform a fold on the tree from left to right; using the same argument order as `List.foldl`.
-}
foldLeft : (a -> b -> b) -> b -> Tree a tag -> b
foldLeft f init (Tree _ t) =
    Internal.foldl (\acc ( _, item ) -> f item acc) init t


{-| Perform a fold on the tree from right to left; using the same argument order as `List.foldr`.
-}
foldRight : (a -> b -> b) -> b -> Tree a tag -> b
foldRight f init (Tree _ t) =
    Internal.foldr (\( _, a ) b -> f a b) t init


{-| This function counts the number of elements in the tree by visiting each element. You could also use the annotation to keep track of this.
-}
length : Tree a tag -> Int
length (Tree _ t) =
    Internal.count t


{-| Extract the tree's annotation. There a few things to keep in mind here:

  - If the tree is empty the empty annotation of the tree's config is returned.
  - This is the cumulative combination / sum of the annotations of the tree's elements.

-}
annotation : Tree a tag -> tag
annotation (Tree c t) =
    Internal.tagOfTree c t


{-| Check if two trees are equal by comparing their elements from left to right.
-}
equal : Tree a tag -> Tree a tag -> Bool
equal (Tree c t1) (Tree _ t2) =
    Internal.equal c t1 t2


{-| Like `viewLeft` but also returns the annotation of the element.
-}
unconsLeft : Tree a tag -> Maybe ( Tagged a tag, Tree a tag )
unconsLeft (Tree c t) =
    Internal.viewLeft c t |> Maybe.map (\( a, rest ) -> ( a, Tree c rest ))


{-| Like `viewRight` but also returns the annotation of the element.
-}
unconsRight : Tree a tag -> Maybe ( Tagged a tag, Tree a tag )
unconsRight (Tree c t) =
    Internal.viewRight c t |> Maybe.map (\( a, rest ) -> ( a, Tree c rest ))


{-| Like `split` but also returns the annotation of the element which caused the split.
-}
cutWithTag : (tag -> Bool) -> Tree a tag -> ( Tree a tag, Maybe (Tagged a tag), Tree a tag )
cutWithTag p (Tree c t) =
    let
        ( l, r ) =
            Internal.splitTree c p c.zero t
    in
    case Internal.viewLeft c r of
        Just ( x, rest ) ->
            ( Tree c l, Just x, Tree c rest )

        Nothing ->
            ( Tree c l, Nothing, Tree c r )


{-| Like `foldLeft` but uses the natural argument order and uses the annotation and element to fold.
-}
foldLeftWithTag : (b -> Tagged a tag -> b) -> b -> Tree a tag -> b
foldLeftWithTag f init (Tree _ t) =
    Internal.foldl f init t


{-| Like `foldRight` but uses the natural argument order and uses the annotation and element to fold.
-}
foldRightWithTag : (Tagged a tag -> b -> b) -> Tree a tag -> b -> b
foldRightWithTag f (Tree _ t) init =
    Internal.foldr f t init
