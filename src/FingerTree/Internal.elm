module FingerTree.Internal exposing (..)


type alias Config a tag =
    { zero : tag, combine : tag -> tag -> tag, tag : a -> tag }


type Node a tag
    = Tip a tag
    | Node2 tag (Node a tag) (Node a tag)
    | Node3 tag (Node a tag) (Node a tag) (Node a tag)


type Digit a tag
    = One (Node a tag)
    | Two (Node a tag) (Node a tag)
    | Three (Node a tag) (Node a tag) (Node a tag)
    | Four (Node a tag) (Node a tag) (Node a tag) (Node a tag)


type Tree a tag
    = Empty
    | Single (Node a tag)
    | Deep tag (Digit a tag) (Tree a tag) (Digit a tag)


tagOfNode : Node a tag -> tag
tagOfNode node =
    case node of
        Tip _ t ->
            t

        Node2 t _ _ ->
            t

        Node3 t _ _ _ ->
            t


onto : (b -> b -> c) -> (a -> b) -> a -> a -> c
onto pair lift a b =
    pair (lift a) (lift b)


onto3 : (b -> b -> b) -> (a -> b) -> a -> a -> a -> b
onto3 pair lift a b c =
    pair (pair (lift a) (lift b)) (lift c)


onto4 : (b -> b -> b) -> (a -> b) -> a -> a -> a -> a -> b
onto4 pair lift a b c d =
    pair (pair (lift a) (lift b)) (pair (lift c) (lift d))


a3 : (a -> b -> a) -> a -> b -> b -> a
a3 f a b c =
    f (f a b) c


tagOfDigit : Config a tag -> Digit a tag -> tag
tagOfDigit { combine } digit =
    case digit of
        One node ->
            tagOfNode node

        Two a b ->
            onto combine tagOfNode a b

        Three a b c ->
            onto3 combine tagOfNode a b c

        Four a b c d ->
            onto4 combine tagOfNode a b c d


tagOfTree : Config a tag -> Tree a tag -> tag
tagOfTree { zero } tree =
    case tree of
        Empty ->
            zero

        Single node ->
            tagOfNode node

        Deep tag _ _ _ ->
            tag


isLevelN : Int -> Node a tag -> Bool
isLevelN n node =
    if n == 0 then
        case node of
            Tip _ _ ->
                True

            _ ->
                False

    else
        case node of
            Node2 _ n1 n2 ->
                isLevelN (n - 1) n1 && isLevelN (n - 1) n2

            Node3 _ n1 n2 n3 ->
                isLevelN (n - 1) n1 && isLevelN (n - 1) n2 && isLevelN (n - 1) n3

            _ ->
                False


isLevelNDigit : Int -> Digit a tag -> Bool
isLevelNDigit n digit =
    case digit of
        One n1 ->
            isLevelN n n1

        Two n1 n2 ->
            isLevelN n n1 && isLevelN n n2

        Three n1 n2 n3 ->
            isLevelN n n1 && isLevelN n n2 && isLevelN n n3

        Four n1 n2 n3 n4 ->
            isLevelN n n1 && isLevelN n n2 && isLevelN n n3 && isLevelN n n4


isLevelNTree : Int -> Tree a tag -> Bool
isLevelNTree n tree =
    case tree of
        Empty ->
            True

        Single node ->
            isLevelN n node

        Deep _ l t r ->
            isLevelNDigit n l && isLevelNDigit n r && isLevelNTree (n + 1) t


isMeasuredNode : Config a tag -> Node a tag -> Bool
isMeasuredNode ({ combine } as config) node =
    case node of
        Tip _ _ ->
            True

        Node2 a n1 n2 ->
            isMeasuredNode config n1 && isMeasuredNode config n2 && (a == onto combine tagOfNode n1 n2)

        Node3 a n1 n2 n3 ->
            isMeasuredNode config n1 && isMeasuredNode config n2 && isMeasuredNode config n3 && (a == onto3 combine tagOfNode n1 n2 n3)


isMeasuredDigit : Config a tag -> Digit a tag -> Bool
isMeasuredDigit config digit =
    case digit of
        One n1 ->
            isMeasuredNode config n1

        Two n1 n2 ->
            isMeasuredNode config n1 && isMeasuredNode config n2

        Three n1 n2 n3 ->
            isMeasuredNode config n1 && isMeasuredNode config n2 && isMeasuredNode config n3

        Four n1 n2 n3 n4 ->
            isMeasuredNode config n1 && isMeasuredNode config n2 && isMeasuredNode config n3 && isMeasuredNode config n4


isMeasuredTree : Config a tag -> Tree a tag -> Bool
isMeasuredTree ({ combine } as config) tree =
    case tree of
        Empty ->
            True

        Single node ->
            isMeasuredNode config node

        Deep a l t r ->
            isMeasuredDigit config l
                && isMeasuredDigit config r
                && isMeasuredTree config t
                && (a == a3 combine (tagOfDigit config l) (tagOfTree config t) (tagOfDigit config r))


nodeToList : Node a tag -> List ( tag, a )
nodeToList node =
    case node of
        Tip a tag ->
            [ ( tag, a ) ]

        Node2 _ n1 n2 ->
            nodeToList n1 ++ nodeToList n2

        Node3 _ n1 n2 n3 ->
            nodeToList n1 ++ nodeToList n2 ++ nodeToList n3


digitToList : Digit a tag -> List ( tag, a )
digitToList digit =
    case digit of
        One n1 ->
            nodeToList n1

        Two n1 n2 ->
            nodeToList n1 ++ nodeToList n2

        Three n1 n2 n3 ->
            nodeToList n1 ++ nodeToList n2 ++ nodeToList n3

        Four n1 n2 n3 n4 ->
            nodeToList n1 ++ nodeToList n2 ++ nodeToList n3 ++ nodeToList n4


treeToList : Tree a tag -> List ( tag, a )
treeToList tree =
    case tree of
        Empty ->
            []

        Single n ->
            nodeToList n

        Deep _ l t r ->
            digitToList l ++ treeToList t ++ digitToList r


deep : Config a tag -> Digit a tag -> Tree a tag -> Digit a tag -> Tree a tag
deep ({ combine } as config) l m r =
    Deep (a3 combine (tagOfDigit config l) (tagOfTree config m) (tagOfDigit config r)) l m r


node2 : Config a tag -> Node a tag -> Node a tag -> Node a tag
node2 { combine } n1 n2 =
    Node2 (combine (tagOfNode n1) (tagOfNode n2)) n1 n2


node3 : Config a tag -> Node a tag -> Node a tag -> Node a tag -> Node a tag
node3 { combine } n1 n2 n3 =
    Node3 (a3 combine (tagOfNode n1) (tagOfNode n2) (tagOfNode n3)) n1 n2 n3


nlcons : Config a tag -> Node a tag -> Tree a tag -> Tree a tag
nlcons config a tree =
    case tree of
        Empty ->
            Single a

        Single b ->
            deep config (One a) Empty (One b)

        Deep _ (One b) m r ->
            deep config (Two a b) m r

        Deep _ (Two b c) m r ->
            deep config (Three a b c) m r

        Deep _ (Three b c d) m r ->
            deep config (Four a b c d) m r

        Deep _ (Four b c d e) m r ->
            deep config (Two a b) (nlcons config (node3 config c d e) m) r


nrcons : Config a tag -> Node a tag -> Tree a tag -> Tree a tag
nrcons config a tree =
    case tree of
        Empty ->
            Single a

        Single b ->
            deep config (One b) Empty (One a)

        Deep _ l m (One b) ->
            deep config l m (Two b a)

        Deep _ l m (Two b c) ->
            deep config l m (Three b c a)

        Deep _ l m (Three b c d) ->
            deep config l m (Four b c d a)

        Deep _ l m (Four b c d e) ->
            deep config l (nrcons config (node3 config b c d) m) (Two e a)


lcons : Config a tag -> a -> Tree a tag -> Tree a tag
lcons ({ tag } as config) a tree =
    nlcons config (Tip a (tag a)) tree


rcons : Config a tag -> a -> Tree a tag -> Tree a tag
rcons ({ tag } as config) a tree =
    nrcons config (Tip a (tag a)) tree


ofList : Config a tag -> List a -> Tree a tag
ofList config list =
    let
        toList l =
            case l of
                [] ->
                    Empty

                a :: xs ->
                    lcons config a (toList xs)
    in
    toList list


digitToTree : Config a tag -> Digit a tag -> Tree a tag
digitToTree config digit =
    case digit of
        One a ->
            Single a

        Two a b ->
            deep config (One a) Empty (One b)

        Three a b c ->
            deep config (Two a b) Empty (One c)

        Four a b c d ->
            deep config (Two a b) Empty (Two c d)


nodeToDigit : Config a tag -> Node a tag -> Digit a tag
nodeToDigit _ node =
    case node of
        Tip a tag ->
            One (Tip a tag)

        Node2 _ a b ->
            Two a b

        Node3 _ a b c ->
            Three a b c


viewLeftNode : Config a tag -> Tree a tag -> Maybe ( Node a tag, Tree a tag )
viewLeftNode config tree =
    case tree of
        Empty ->
            Nothing

        Single a ->
            Just ( a, Empty )

        Deep _ (Two a b) m r ->
            Just ( a, deep config (One b) m r )

        Deep _ (Three a b c) m r ->
            Just ( a, deep config (Two b c) m r )

        Deep _ (Four a b c d) m r ->
            Just ( a, deep config (Three b c d) m r )

        Deep _ (One a) m r ->
            case viewLeftNode config m of
                Nothing ->
                    Just ( a, digitToTree config r )

                Just ( b, m2 ) ->
                    Just ( a, deep config (nodeToDigit config b) m2 r )


viewRightNode : Config a tag -> Tree a tag -> Maybe ( Node a tag, Tree a tag )
viewRightNode config tree =
    case tree of
        Empty ->
            Nothing

        Single a ->
            Just ( a, Empty )

        Deep _ l m (Two a b) ->
            Just ( b, deep config l m (One a) )

        Deep _ l m (Three a b c) ->
            Just ( c, deep config l m (Two a b) )

        Deep _ l m (Four a b c d) ->
            Just ( d, deep config l m (Three a b c) )

        Deep _ l m (One a) ->
            case viewRightNode config m of
                Nothing ->
                    Just ( a, digitToTree config l )

                Just ( b, m2 ) ->
                    Just ( a, deep config l m2 (nodeToDigit config b) )


nodeToEl : Node a tag -> Maybe ( tag, a )
nodeToEl node =
    case node of
        Tip a tag ->
            Just ( tag, a )

        _ ->
            Nothing


pairWith : b -> a -> ( a, b )
pairWith b a =
    ( a, b )


viewLeft : Config a tag -> Tree a tag -> Maybe ( ( tag, a ), Tree a tag )
viewLeft config tree =
    case viewLeftNode config tree of
        Just ( na, rest ) ->
            nodeToEl na |> Maybe.map (pairWith rest)

        Nothing ->
            Nothing


viewRight : Config a tag -> Tree a tag -> Maybe ( ( tag, a ), Tree a tag )
viewRight config tree =
    case viewRightNode config tree of
        Just ( na, rest ) ->
            nodeToEl na |> Maybe.map (pairWith rest)

        Nothing ->
            Nothing


head : Config a tag -> Tree a tag -> Maybe a
head config tree =
    viewLeft config tree |> Maybe.map (Tuple.first >> Tuple.second)


tail : Config a tag -> Tree a tag -> Maybe (Tree a tag)
tail config tree =
    viewLeft config tree |> Maybe.map Tuple.second


end : Config a tag -> Tree a tag -> Maybe a
end config tree =
    viewRight config tree |> Maybe.map (Tuple.first >> Tuple.second)


prefix : Config a tag -> Tree a tag -> Maybe (Tree a tag)
prefix config tree =
    viewRight config tree |> Maybe.map Tuple.second



--


lconsNodes : Config a tag -> List (Node a tag) -> Tree a tag -> Tree a tag
lconsNodes config nl tree =
    case nl of
        [] ->
            tree

        x :: xs ->
            nlcons config x (lconsNodes config xs tree)


rconsNodes : Config a tag -> List (Node a tag) -> Tree a tag -> Tree a tag
rconsNodes config nl tree =
    case nl of
        [] ->
            tree

        x :: xs ->
            rconsNodes config xs (nrcons config x tree)


nodes : Config a tag -> ( Node a tag, Node a tag, List (Node a tag) ) -> List (Node a tag)
nodes config ( a, b, nl ) =
    case nl of
        [] ->
            [ node2 config a b ]

        [ c ] ->
            [ node3 config a b c ]

        [ c, d ] ->
            [ node2 config a b, node2 config c d ]

        c :: d :: e :: more ->
            node3 config a b c :: nodes config ( d, e, more )


digitToNonempty : Digit a tag -> ( Node a tag, List (Node a tag) )
digitToNonempty digit =
    case digit of
        One a ->
            ( a, [] )

        Two a b ->
            ( a, [ b ] )

        Three a b c ->
            ( a, [ b, c ] )

        Four a b c d ->
            ( a, [ b, c, d ] )


join : ( a, List a ) -> ( a, List a ) -> ( a, a, List a )
join la lb =
    case ( la, lb ) of
        ( ( a, [] ), ( b, rest ) ) ->
            ( a, b, rest )

        ( ( a, x :: xs ), ( b, rest ) ) ->
            ( a, x, xs ++ (b :: rest) )


cat : ( a, List a ) -> List a -> ( a, List a )
cat ( x, xs ) rest =
    ( x, xs ++ rest )


buildNodes : Config a tag -> Digit a tag -> List (Node a tag) -> Digit a tag -> List (Node a tag)
buildNodes config l ts r =
    nodes config (join (cat (digitToNonempty l) ts) (digitToNonempty r))


app3 : Config a tag -> Tree a tag -> List (Node a tag) -> Tree a tag -> Tree a tag
app3 config tA xs_ tB =
    case ( tA, xs_, tB ) of
        ( Empty, xs, t ) ->
            lconsNodes config xs t

        ( t, xs, Empty ) ->
            rconsNodes config xs t

        ( Single x, xs, t ) ->
            nlcons config x (lconsNodes config xs t)

        ( t, xs, Single x ) ->
            nrcons config x (rconsNodes config xs t)

        ( Deep _ l1 m1 r1, ts, Deep _ l2 m2 r2 ) ->
            deep config l1 (app3 config m1 (buildNodes config r1 ts l2) m2) r2


append : Config a tag -> Tree a tag -> Tree a tag -> Tree a tag
append config t1 t2 =
    app3 config t1 [] t2


deepL : Config a tag -> Maybe (Digit a tag) -> Tree a tag -> Digit a tag -> Tree a tag
deepL config ll m r =
    case ll of
        Nothing ->
            case viewLeftNode config m of
                Nothing ->
                    digitToTree config r

                Just ( a, m2 ) ->
                    deep config (nodeToDigit config a) m2 r

        Just l ->
            deep config l m r


deepR : Config a tag -> Digit a tag -> Tree a tag -> Maybe (Digit a tag) -> Tree a tag
deepR config l m rr =
    case rr of
        Nothing ->
            case viewRightNode config m of
                Nothing ->
                    digitToTree config l

                Just ( a, m2 ) ->
                    deep config l m2 (nodeToDigit config a)

        Just r ->
            deep config l m r


cutDigit : Config a tag -> (tag -> Bool) -> tag -> Digit a tag -> ( Maybe (Digit a tag), Maybe (Digit a tag) )
cutDigit { combine } p i digit =
    case digit of
        One a ->
            if p (combine i (tagOfNode a)) then
                ( Nothing, Just digit )

            else
                ( Just digit, Nothing )

        Two a b ->
            let
                i2 =
                    combine i (tagOfNode a)
            in
            if p i2 then
                ( Nothing, Just (Two a b) )

            else
                ( Just (One a), Just (One b) )

        Three a b c ->
            let
                i2 =
                    combine i (tagOfNode a)
            in
            if p i2 then
                ( Nothing, Just (Three a b c) )

            else
                let
                    i3 =
                        combine i2 (tagOfNode b)
                in
                if p i3 then
                    ( Just (One a), Just (Two b c) )

                else
                    let
                        i4 =
                            combine i3 (tagOfNode c)
                    in
                    if p i4 then
                        ( Just (Two a b), Just (One c) )

                    else
                        ( Just (Three a b c), Nothing )

        Four a b c d ->
            let
                i2 =
                    combine i (tagOfNode a)
            in
            if p i2 then
                ( Nothing, Just (Four a b c d) )

            else
                let
                    i3 =
                        combine i2 (tagOfNode b)
                in
                if p i3 then
                    ( Just (One a), Just (Three b c d) )

                else
                    let
                        i4 =
                            combine i3 (tagOfNode c)
                    in
                    if p i4 then
                        ( Just (Two a b), Just (Two c d) )

                    else
                        let
                            i5 =
                                combine i4 (tagOfNode d)
                        in
                        if p i5 then
                            ( Just (Three a b c), Just (One d) )

                        else
                            ( Just (Four a b c d), Nothing )


splitTree : Config a tag -> (tag -> Bool) -> tag -> Tree a tag -> ( Tree a tag, Tree a tag )
splitTree ({ combine } as config) p i t =
    case t of
        Empty ->
            ( Empty, Empty )

        Single a ->
            if p (combine i (tagOfNode a)) then
                ( Empty, Single a )

            else
                ( Single a, Empty )

        Deep _ l m r ->
            let
                vl =
                    combine i (tagOfDigit config l)

                vm =
                    combine vl (tagOfTree config m)
            in
            if p vl then
                let
                    ( left, right ) =
                        cutDigit config p i l
                in
                ( left |> Maybe.map (digitToTree config) |> Maybe.withDefault Empty, deepL config right m r )

            else if p vm then
                let
                    ( ml, mr ) =
                        splitTree config p vl m
                in
                case viewLeftNode config mr of
                    Just ( xs, mrr ) ->
                        let
                            ( lf, rf ) =
                                cutDigit config p (combine vl (tagOfTree config ml)) (nodeToDigit config xs)
                        in
                        ( deepR config l ml lf, deepL config rf mr r )

                    Nothing ->
                        ( deepR config l ml Nothing, deepL config Nothing mr r )

            else
                let
                    ( left, right ) =
                        cutDigit config p vm r
                in
                ( deepR config l m left, right |> Maybe.map (digitToTree config) |> Maybe.withDefault Empty )


foldLNode : (b -> ( tag, a ) -> b) -> b -> Node a tag -> b
foldLNode f s n =
    case n of
        Tip a tag ->
            f s ( tag, a )

        Node2 _ a b ->
            foldLNode f (foldLNode f s a) b

        Node3 _ a b c ->
            foldLNode f (foldLNode f (foldLNode f s a) b) c


foldLDigit : (b -> ( tag, a ) -> b) -> b -> Digit a tag -> b
foldLDigit f s digit =
    case digit of
        One a ->
            foldLNode f s a

        Two a b ->
            foldLNode f (foldLNode f s a) b

        Three a b c ->
            foldLNode f (foldLNode f (foldLNode f s a) b) c

        Four a b c d ->
            foldLNode f (foldLNode f (foldLNode f (foldLNode f s a) b) c) d


foldRNode : (( tag, a ) -> b -> b) -> Node a tag -> b -> b
foldRNode f n s =
    case n of
        Tip a tag ->
            f ( tag, a ) s

        Node2 _ a b ->
            foldRNode f a (foldRNode f b s)

        Node3 _ a b c ->
            foldRNode f a (foldRNode f b (foldRNode f c s))


foldRDigit : (( tag, a ) -> b -> b) -> Digit a tag -> b -> b
foldRDigit f digit s =
    case digit of
        One a ->
            foldRNode f a s

        Two a b ->
            foldRNode f a (foldRNode f b s)

        Three a b c ->
            foldRNode f a (foldRNode f b (foldRNode f c s))

        Four a b c d ->
            foldRNode f a (foldRNode f b (foldRNode f c (foldRNode f d s)))


foldl : (s -> ( tag, a ) -> s) -> s -> Tree a tag -> s
foldl f s t =
    case t of
        Empty ->
            s

        Single a ->
            foldLNode f s a

        Deep _ l m r ->
            foldLDigit f (foldl f (foldLDigit f s l) m) r


foldr : (( tag, a ) -> s -> s) -> Tree a tag -> s -> s
foldr f t s =
    case t of
        Empty ->
            s

        Single a ->
            foldRNode f a s

        Deep _ l m r ->
            foldRDigit f l (foldr f m (foldRDigit f r s))


countNodeHelp : Node a tag -> List (Node a tag) -> Int -> Int
countNodeHelp node q acc =
    case node of
        Tip _ _ ->
            case q of
                [] ->
                    1 + acc

                next :: qs ->
                    countNodeHelp next qs (acc + 1)

        Node2 _ a b ->
            countNodeHelp a (b :: q) acc

        Node3 _ a b c ->
            countNodeHelp a (b :: c :: q) acc


countNode : Node a tag -> Int
countNode node =
    countNodeHelp node [] 0


countDigit : Digit a tag -> Int
countDigit digit =
    case digit of
        One a ->
            countNode a

        Two a b ->
            countNodeHelp a [ b ] 0

        Three a b c ->
            countNodeHelp a [ b, c ] 0

        Four a b c d ->
            countNodeHelp a [ b, c, d ] 0


count : Tree a tag -> Int
count tree =
    case tree of
        Empty ->
            0

        Single a ->
            countNode a

        Deep _ l m r ->
            countDigit l + count m + countDigit r


equal : Config a tag -> Tree a tag -> Tree a tag -> Bool
equal config t1 t2 =
    case ( viewLeft config t1, viewLeft config t2 ) of
        ( Nothing, Nothing ) ->
            True

        ( Just ( ( tagX, x ), lrest ), Just ( ( tagY, y ), rrest ) ) ->
            if x == y && tagX == tagY then
                equal config lrest rrest

            else
                False

        _ ->
            False
