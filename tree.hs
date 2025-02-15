import Control.Exception (Exception, catch, throw)
import Data.Function ((&))
import GHC.Internal.Stack (HasCallStack, CallStack, callStack, prettyCallStack)
import Text.Printf (printf)
import Distribution.Compat.Prelude (isNothing)

main :: IO ()
main = do
    let myTree :: Tree Double =
            makeTree 5
                `setLeftChild`
                    (makeTree 3
                        `setLeftChild` makeTree (-1))
                `setRightChild`
                    makeTree 2
    printf "tree             :           %s\n" . show $ myTree
    printf "left             :           %s\n" . show $ myTree & left
    printf "left left        :           %s\n" . show $ myTree & left >>= left
    printf "left left left   :           %s\n" . show $ myTree & left >>= left >>= left
    printf "left left right  :           %s\n" . show $ myTree & left >>= left >>= right
    printf "right            :           %s\n" . show $ myTree & right
    printf "right left       :           %s\n" . show $ myTree & right >>= left
    printf "right right      :           %s\n" . show $ myTree & right >>= right
    printf "right right right:           %s\n" . show $ myTree & right >>= right >>= right
    print $ sum myTree
    print $ foldr (/) 10 myTree  -- = 5 / (3 / (-1 / (2 / 10))) = -8.3...
    print $ foldl' (/) 10 myTree -- = 2 / (3 / (-1 / (10 / 5))) = -0.3...
    catch (print .  setRightChild myTree $ makeTree(-100)) (printHandler :: TreeException -> IO ())
    print . removeLeftChild . removeRightChild $ myTree
    catch (print . removeLeftChild . removeRightChild . removeLeftChild $ myTree) (printHandler :: TreeException -> IO ())
    let sortedTree :: Tree Double = sortTree myTree
    printf "Sorted tree:                 %s\n" . show $ sortedTree
    printf "Sorted tree left:            %s\n" . show $ sortedTree
    printf "Sorted tree left left:       %s\n" . show $ sortedTree & left >>= left
    printf "Sorted tree left left right: %s\n" . show $ sortedTree & left >>= left >>= right
    printf "One-node sorted tree:        %s\n" . show . sortTree . makeTree $ 5
    let largerTree :: Tree Double =
            makeTree 4
                `setLeftChild`
                    (makeTree 6
                        `setLeftChild` makeTree 7
                        `setRightChild` makeTree 5)
                `setRightChild`
                    (makeTree 2
                        `setLeftChild` makeTree 3
                        `setRightChild` makeTree 1)
    printf "larger tree:                 %s\n" . show $ largerTree
    printf "larger sorted tree:          %s\n" . show . sortTree $ largerTree
    where
        printHandler :: (Exception t) => t -> IO ()
        printHandler = print

data TreeException = TreeException { message :: String, stack :: CallStack}

instance Show TreeException where
    show :: TreeException -> String
    show (TreeException message callStack) = "*** Error: " ++ message ++
        case prettyCallStack callStack of
            "" -> ""
            value -> "\n" ++ value

instance Exception TreeException

data Tree t = Tree { value :: t, left :: Maybe (Tree t), right :: Maybe (Tree t) }
    deriving (Eq, Foldable)

instance (Show t) => Show (Tree t) where
    show :: Tree t -> String
    show (Tree value left right) =
        printf
            "(value = %s%s%s)"
            (show value)
            (case left of
                Nothing -> ""
                (Just tree) -> printf ", left = %s" (show tree))
            (case right of
                Nothing -> ""
                (Just tree) -> printf ", right = %s" (show tree))

sortTree :: (Ord t) => Tree t -> Tree t
sortTree tree@(Tree _ Nothing Nothing) = tree
sortTree (Tree value (Just left) Nothing) = insertTreeSorted (makeTree value) left
sortTree (Tree value Nothing (Just right)) = insertTreeSorted (makeTree value) right
sortTree (Tree value (Just left) (Just right)) =
    insertTreeSorted
        (insertTreeSorted (makeTree value) left)
        right

insertTreeSorted :: (Ord t) => Tree t -> Tree t -> Tree t
insertTreeSorted tree (Tree valueToInsert Nothing Nothing) = insertValueSorted tree valueToInsert
insertTreeSorted tree (Tree valueToInsert (Just leftToInsert) Nothing) =
    insertTreeSorted
        (insertValueSorted tree valueToInsert)
        leftToInsert
insertTreeSorted tree (Tree valueToInsert Nothing (Just rightToInsert)) =
    insertTreeSorted
        (insertValueSorted tree valueToInsert)
        rightToInsert
insertTreeSorted tree (Tree valueToInsert (Just leftToInsert) (Just rightToInsert)) =
    insertTreeSorted
        (insertTreeSorted
            (insertValueSorted tree valueToInsert)
            leftToInsert)
        rightToInsert

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

makeTree :: t -> Tree t
makeTree value = Tree value Nothing Nothing

setLeftChild :: HasCallStack => Tree t ->  Tree t -> Tree t
setLeftChild (Tree value Nothing right) leftChild = Tree value (Just leftChild) right
setLeftChild _ _ = throw $ TreeException "Tree already has a left child" callStack

setRightChild :: HasCallStack => Tree t -> Tree t -> Tree t
setRightChild (Tree value left Nothing) rightChild = Tree value left (Just rightChild)
setRightChild _ _ = throw $ TreeException "Tree already has a right child" callStack

removeLeftChild :: HasCallStack => Tree t -> Tree t
removeLeftChild (Tree value (Just _) right) = Tree value Nothing right
removeLeftChild Tree {} = throw $ TreeException "Tree has no left child" callStack

removeRightChild :: HasCallStack => Tree t -> Tree t
removeRightChild (Tree value left (Just _)) = Tree value left Nothing
removeRightChild Tree {} = throw $ TreeException "Tree has no right child" callStack