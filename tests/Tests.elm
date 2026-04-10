module Tests exposing (suite)

import Expect
import FingerTree as Ft
import Fuzz
import Test exposing (Test, describe, test)


simple =
    Ft.create { annotate = always 1, combine = (+), empty = 0 }


tempty =
    Ft.empty simple


fuzzer =
    let
        conser x =
            Fuzz.oneOfValues [ Ft.consLeft x, Ft.consRight x ]
    in
    Fuzz.listOfLengthBetween 1 32 Fuzz.int
        |> Fuzz.andThen
            (\elements ->
                elements |> Fuzz.traverse conser |> Fuzz.map (List.foldl (<|) tempty)
            )


fuzzerWithElt =
    fuzzer |> Fuzz.andThen (\tree -> Fuzz.uniformInt (Ft.length tree - 1) |> Fuzz.map (\e -> ( tree, 1 + e )))


suite : Test
suite =
    describe "The FingerTree module"
        [ describe "empty"
            [ test "produces trees that have a zero count of elements" <|
                \_ -> Expect.equal 0 (Ft.empty simple |> Ft.length)
            , test "produces the same tree per config" <|
                \_ -> Expect.equal (Ft.empty simple) (Ft.empty simple)
            , test "has the empty annotation" <|
                \_ -> Expect.equal 0 (Ft.empty simple |> Ft.annotation)
            , test "produces the empty list" <|
                \_ -> Expect.equalLists [] (Ft.empty simple |> Ft.toList)
            ]
        , describe "Adding elements with"
            [ test "consLeft and consRight on the empty tree produces the same tree" <|
                \_ ->
                    Expect.equal (Ft.consLeft 1 tempty) (Ft.consRight 1 tempty)
            , test "consLeft adds to the front of the tree" <|
                \_ ->
                    Expect.equalLists [ 1, 2, 3 ]
                        (Ft.toList <|
                            Ft.consLeft 1 <|
                                Ft.consLeft 2 <|
                                    Ft.consLeft 3 tempty
                        )
            , test "consRight adds to the end of the tree" <|
                \_ ->
                    Expect.equalLists [ 4, 5, 6 ]
                        (tempty
                            |> Ft.consRight 4
                            |> Ft.consRight 5
                            |> Ft.consRight 6
                            |> Ft.toList
                        )
            , test "appendList appends a list to the end of the tree" <|
                \_ ->
                    Expect.equalLists [ 4, 1, 2, 3 ]
                        (tempty
                            |> Ft.consLeft 4
                            |> Ft.appendList [ 1, 2, 3 ]
                            |> Ft.toList
                        )
            , test "prependList adds elements to the start" <|
                \_ ->
                    Expect.equalLists [ 1, 2, 3, 4 ]
                        (tempty
                            |> Ft.consLeft 4
                            |> Ft.prependList [ 1, 2, 3 ]
                            |> Ft.toList
                        )
            ]
        , describe "viewLeft"
            [ Test.fuzz fuzzer "extracts the head of the tree" <|
                \tree ->
                    case Ft.viewLeft (Ft.consLeft 1 tree) of
                        Just ( x, rest ) ->
                            case ( x == 1, Ft.equal tree rest ) of
                                ( True, True ) ->
                                    Expect.pass

                                ( False, _ ) ->
                                    Expect.fail "head was not equal to left cons"

                                ( _, False ) ->
                                    Expect.fail "rest was not equal to original tree"

                        Nothing ->
                            Expect.fail "cons on an empty tree should not be empty"
            ]
        , describe "viewRight"
            [ Test.fuzz fuzzer "extracts the end of the tree" <|
                \tree ->
                    case Ft.viewRight (Ft.consRight 1 tree) of
                        Just ( x, prefix ) ->
                            case ( x == 1, Ft.equal tree prefix ) of
                                ( True, True ) ->
                                    Expect.pass

                                ( False, _ ) ->
                                    Expect.fail "end was not equal to right cons"

                                ( _, False ) ->
                                    Expect.fail "prefix was not equal to original tree"

                        Nothing ->
                            Expect.fail "cons on an empty tree should not be empty"
            ]
        , describe "isEmpty"
            [ Test.fuzz fuzzer "is false for a non empty tree" <|
                \tree -> Expect.notEqual True (Ft.isEmpty tree)
            , test "is true for an empty tree" <| \_ -> Expect.equal True (Ft.isEmpty tempty)
            , Test.fuzz fuzzer "is true for a tree that was emptied" <|
                \tree ->
                    let
                        drain t =
                            case Ft.viewLeft t of
                                Nothing ->
                                    t

                                Just ( _, r ) ->
                                    drain r
                    in
                    Expect.equal True (Ft.isEmpty (drain tree))
            ]
        , describe "append"
            [ Test.fuzz2 fuzzer fuzzer "appends two trees left to right" <|
                \tree1 tree2 ->
                    let
                        result =
                            Ft.append tree1 tree2
                    in
                    Expect.equalLists (List.append (Ft.toList tree1) (Ft.toList tree2)) (Ft.toList result)
            ]
        , describe "split"
            [ test "split using the size annotation splits at the nth element" <|
                \_ ->
                    let
                        tree =
                            tempty |> Ft.appendList [ 1, 2, 3, 6, 7, 8 ]
                    in
                    case Ft.cut (\i -> i >= 4) tree of
                        ( _, Just x, _ ) ->
                            Expect.equal 6 x

                        _ ->
                            Expect.fail "non empty tree split should not fail"
            , Test.fuzz fuzzerWithElt "the sum of the lengths the splits is the length of the source" <|
                \( tree, e ) ->
                    let
                        ( l, r ) =
                            Ft.splitAt e tree
                    in
                    Expect.equal (Ft.length l + Ft.length r) (Ft.length tree)
            ]
        , describe "equal"
            [ Test.fuzz fuzzer "only the empty tree is equal to itself" <|
                \tree -> Expect.equal False (Ft.equal tree tempty)
            , test "two trees are equal if their contents and annotations are equal" <|
                \_ ->
                    let
                        t1 =
                            tempty |> Ft.appendList [ 1, 2, 3, 4 ]

                        t2 =
                            tempty |> Ft.prependList [ 1, 2, 3, 4 ]
                    in
                    Expect.equal True (Ft.equal t1 t2)
            ]
        , describe "annotation"
            [ test "is the annotation of the tree" <|
                \_ -> Expect.equal 5 (tempty |> Ft.appendList [ 1, 2, 3, 4, 5 ] |> Ft.annotation)
            ]
        ]
