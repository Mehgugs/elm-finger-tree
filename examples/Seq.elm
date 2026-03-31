module Seq exposing (..)

import FingerTree as Ft


size : Ft.Config a Int
size =
    Ft.create
        { empty = 0
        , combine = (+)
        , annotate = always 1
        }


type alias Seq a =
    Ft.Tree a Int


empty : Seq a
empty =
    Ft.empty size


fromList : List a -> Seq a
fromList l =
    empty |> Ft.appendList l


nth : Int -> Seq a -> Maybe a
nth n sq =
    Ft.splitAt n sq |> Tuple.second |> Ft.head
