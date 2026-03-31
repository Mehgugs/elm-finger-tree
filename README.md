# elm-finger-tree

An implementation of [finger trees][wikipedia page] in elm!

This package implements the data structure according to the [isabelle proof] with some deviations to ensure that functions are total.
Following elm style, the head/tail style functions return their results wrapped in `Maybe` and the higher level fold operations follow elm's `List.foldl`
argument ordering.



# Examples 

There's a small collection of rough examples in the `examples/` folder which are based on the applications section of the [original paper]. 
The simplest of these is the random access sequence, which I've also copied below to showcase how this type can be used to create new data structures.

## Example : Random access sequence

A simple random access sequence, using the split operation to partition the list at the nth element.

```elm
import FingerTree as Ft


type alias Seq a =
    Ft.Tree a Int


size : Ft.Config a Int
size =
    Ft.create
        { empty = 0
        , combine = (+)
        , annotate = always 1
        }


empty : Seq a
empty =
    Ft.empty size


fromList : List a -> Seq a
fromList l =
    empty |> Ft.appendList l


nth : Int -> Seq a -> Maybe a
nth n sq =
    Ft.splitAt n sq |> Tuple.second |> Ft.head

```


[wikipedia page]: https://en.wikipedia.org/wiki/Finger_tree
[isabelle proof]: https://www.isa-afp.org/entries/Finger-Trees.html
[original paper]: https://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf
