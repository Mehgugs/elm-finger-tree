module PQueue exposing (..)

import FingerTree as Ft


type alias PQueue comparable a =
    Ft.Tree a (Priority comparable)


type Priority comparable
    = Min
    | Prio comparable


lessThanEq : Priority comparable -> Priority comparable -> Bool
lessThanEq a b =
    mmax a b == a


mmax : Priority comparable -> Priority comparable -> Priority comparable
mmax a b =
    case ( a, b ) of
        ( Min, _ ) ->
            b

        ( _, Min ) ->
            a

        ( Prio aa, Prio bb ) ->
            Prio (max aa bb)


empty : (a -> comparable) -> PQueue comparable a
empty priority =
    Ft.create { empty = Min, combine = mmax, annotate = priority >> Prio } |> Ft.empty


next : PQueue comparable a -> Maybe ( a, PQueue comparable a )
next q =
    let
        total =
            Ft.annotation q

        ( l, r ) =
            Ft.split (\x -> total |> lessThanEq x) q
    in
    case Ft.viewLeft r of
        Just ( x, rr ) ->
            Just ( x, Ft.append l rr )

        Nothing ->
            Nothing
