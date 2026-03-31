module OrdSeq exposing (..)

import FingerTree as Ft


type alias OrdSeq comparable =
    Ft.Tree comparable (Maybe comparable)


keyed : Ft.Config a (Maybe a)
keyed =
    Ft.create { empty = Nothing, combine = \a b -> b |> Maybe.map Just |> Maybe.withDefault a, annotate = Just }


keyPredicate : (a -> Bool) -> (Maybe a -> Bool)
keyPredicate f =
    Maybe.map f >> Maybe.withDefault False


keyMPredicate : (Maybe a -> Maybe Bool) -> (Maybe a -> Bool)
keyMPredicate f =
    f >> Maybe.withDefault False


empty : OrdSeq comparable
empty =
    Ft.empty keyed


partition : comparable -> OrdSeq comparable -> ( OrdSeq comparable, OrdSeq comparable )
partition k xs =
    Ft.split (keyPredicate (\x -> x >= k)) xs


insert : comparable -> OrdSeq comparable -> OrdSeq comparable
insert x xs =
    let
        ( l, r ) =
            partition x xs
    in
    Ft.append l (Ft.consLeft x r)


deleteAll : comparable -> OrdSeq comparable -> OrdSeq comparable
deleteAll x xs =
    let
        ( l, upper ) =
            partition x xs

        ( _, r ) =
            Ft.split (keyPredicate (\y -> y > x)) upper
    in
    Ft.append l r


merge : OrdSeq comparable -> OrdSeq comparable -> OrdSeq comparable
merge xs ys =
    let
        mergeHelp ps qs =
            case Ft.unconsLeft qs of
                Nothing ->
                    ps

                Just ( ( qtag, q ), qss ) ->
                    let
                        ( l, r ) =
                            Ft.split (keyMPredicate (Maybe.map2 (<=) qtag)) ps
                    in
                    Ft.append l (Ft.consLeft q (mergeHelp qss r))
    in
    mergeHelp xs ys
