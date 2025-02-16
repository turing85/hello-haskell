module Tree.Ord (
    insertTreeSorted,
    insertTreesSorted,
    insertValueSorted,
    insertValuesSorted,
    isSorted,
    sortTree)
    where

import Tree (
    Tree (Tree),
    TreeException,
    makeTree,
    toListPrefix,
    value)
import Data.Type.Ord (OrderingI(LTI))

isSorted :: (Ord t) => Tree t -> Bool
isSorted (Tree value left right) =
    maybe True ((>) value . Tree.value) left &&
    maybe True ((<) value . Tree.value) right &&
    maybe True isSorted left &&
    maybe True isSorted right

sortTree :: (Ord t) => Tree t -> Tree t
sortTree tree = case isSorted tree of 
    True -> tree
    False -> insertValuesSorted (makeTree first) rest
        where (first:rest) = toListPrefix tree

insertTreesSorted :: (Ord t) => Tree t -> [Tree t] -> Tree t
insertTreesSorted = foldl insertTreeSorted

insertTreeSorted :: (Ord t) => Tree t -> Tree t -> Tree t
insertTreeSorted = foldl insertValueSorted

insertValuesSorted :: (Ord t) => Tree t -> [t] -> Tree t
insertValuesSorted = foldl insertValueSorted

insertValueSorted :: (Ord t) => Tree t -> t -> Tree t
insertValueSorted tree@(Tree value left right) toInsert
    | valueCompare == EQ = tree
    | valueCompare == GT = case left of
        Nothing -> Tree value (Just . makeTree $ toInsert) right
        (Just left) -> Tree value (Just . insertValueSorted left $ toInsert) right
    | valueCompare == LT = case right of
        Nothing -> Tree value left (Just .makeTree $ toInsert)
        Just right -> Tree value left (Just . insertValueSorted right $ toInsert)
    where valueCompare = value `compare` toInsert