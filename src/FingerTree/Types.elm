module FingerTree.Types exposing (TreeConfig(..), UnwrappedTree(..))

import FingerTree.Internal as Internal


type TreeConfig a tag
    = Cfg (Internal.Config a tag)


type UnwrappedTree a tag
    = Tree (Internal.Tree a tag)
