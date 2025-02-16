module Tree (
    TreeException,
    Tree (Tree),
    value,
    left,
    right,
    makeTree,
    toListPrefix,
    toListInfix,
    toListSuffix,
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

toListPrefix :: Tree t -> [t]
toListPrefix = toList

toListInfix :: Tree t -> [t]
toListInfix (Tree value left right) =
    maybe [] toListInfix left ++
    [value] ++
    maybe [] toListInfix right

toListSuffix :: Tree t -> [t]
toListSuffix (Tree value left right) =
    maybe [] toListSuffix left ++
    maybe [] toListSuffix right ++
    [value]

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