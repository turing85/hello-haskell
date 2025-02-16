module Tree (
    TreeException,
    Tree,
    value,
    left,
    right,
    makeTree,
    sortTree,
    toListPrefix,
    toListInfix,
    toListSuffix,
    insertTreesSorted,
    insertTreeSorted,
    insertValuesSorted,
    insertValueSorted,
    setLeftChild,
    setRightChild,
    removeLeftChild,
    removeRightChild)
    where

import Control.Exception (Exception, throw)
import GHC.Internal.Stack (HasCallStack, CallStack, callStack, prettyCallStack)
import Distribution.Compat.Prelude (Foldable (toList))

data TreeException = TreeException { message :: String, stack :: CallStack }

instance Show TreeException where
    show :: TreeException -> String
    show (TreeException message callStack) = "*** Error: " ++ message ++
        case prettyCallStack callStack of
            "" -> ""
            value -> "\n" ++ value

instance Exception TreeException

data Tree t = Tree { value :: t, left :: Maybe (Tree t), right :: Maybe (Tree t) }
    deriving (Eq, Foldable, Functor)

instance (Show t) => Show (Tree t) where
    show :: Tree t -> String
    show tree =
        "(value = " ++ (show . value $ tree) ++
        maybe "" ((++) ", left = " . show) (left tree) ++
        maybe "" ((++) ", right = " . show) (right tree) ++
        ")"

makeTree :: t -> Tree t
makeTree value = Tree value Nothing Nothing

sortTree :: (Ord t) => Tree t -> Tree t
sortTree tree = insertValuesSorted (makeTree first) rest
    where (first:rest) = toListPrefix tree

toListPrefix :: Tree t -> [t]
toListPrefix = toList

toListInfix :: Tree t -> [t]
toListInfix tree =
    maybe [] toListInfix (left tree) ++
    [value tree] ++
    maybe [] toListInfix (right tree)

toListSuffix :: Tree t -> [t]
toListSuffix = foldl (flip (:)) ([]::[t])

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
        Nothing -> Tree
            value
            (Just (makeTree toInsert))
            right
        (Just left) -> Tree
            value
            (Just (insertValueSorted left toInsert))
            right
    | valueCompare == LT = case right of
        Nothing -> Tree
            value
            left
            (Just (makeTree toInsert))
        Just right -> Tree
            value
            left
            (Just (insertValueSorted right toInsert))
    where
        valueCompare = value `compare` toInsert

setLeftChild :: HasCallStack => Tree t ->  Tree t -> Tree t
setLeftChild (Tree value Nothing right) leftChild = Tree value (Just leftChild) right
setLeftChild _ _ = throw $ TreeException "Tree already has a left child" callStack

setRightChild :: HasCallStack => Tree t -> Tree t -> Tree t
setRightChild (Tree value left Nothing) rightChild = Tree value left (Just rightChild)
setRightChild _ _ = throw $ TreeException "Tree already has a right child" callStack

removeLeftChild :: HasCallStack => Tree t -> Tree t
removeLeftChild (Tree value (Just _) right) = Tree value Nothing right
removeLeftChild _ = throw $ TreeException "Tree has no left child" callStack

removeRightChild :: HasCallStack => Tree t -> Tree t
removeRightChild (Tree value left (Just _)) = Tree value left Nothing
removeRightChild _ = throw $ TreeException "Tree has no right child" callStack