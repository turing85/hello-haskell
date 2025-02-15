import Control.Exception (Exception, catch, throw)
import Data.Function ((&))
import GHC.Internal.Stack (HasCallStack, CallStack, callStack, prettyCallStack)
import Text.Printf (printf)

main :: IO ()
main = do
    let myTree :: Tree Double =
            makeTree 5
                `setLeftChild`
                    (makeTree 3
                        `setLeftChild` makeTree (-1))
                `setRightChild`
                    makeTree 2
    printf "tree             : %s\n" . show $ myTree
    printf "left             : %s\n" . show $ myTree & left
    printf "left left        : %s\n" . show $ myTree & left >>= left
    printf "left left left   : %s\n" . show $ myTree & left >>= left >>= left
    printf "left left right  : %s\n" . show $ myTree & left >>= left >>= right
    printf "right            : %s\n" . show $ myTree & right
    printf "right left       : %s\n" . show $ myTree & right >>= left
    printf "right right      : %s\n" . show $ myTree & right >>= right
    printf "right right right: %s\n" . show $ myTree & right >>= right >>= right
    print $ sum myTree
    print $ foldr (/) 10 myTree  -- = 5 / (3 / (-1 / (2 / 10)))  = -8.3...
    print $ foldl' (/) 10 myTree -- = 2 / (3 / (-1 / (10 / 5)))  = -0.3...
    catch (print .  setRightChild myTree $ makeTree(-100)) (printHandler :: TreeException -> IO ())
    print . removeLeftChild . removeRightChild $ myTree
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

makeTree :: t -> Tree t
makeTree value = Tree value Nothing Nothing

setLeftChild :: HasCallStack => Tree t ->  Tree t -> Tree t
setLeftChild (Tree value Nothing right) left = Tree value (Just left) right
setLeftChild _ _ = throw $ TreeException "Tree already has a left child" callStack

setRightChild :: HasCallStack => Tree t -> Tree t -> Tree t
setRightChild (Tree value left Nothing) right = Tree value left (Just right)
setRightChild _ _ = throw $ TreeException "Tree already has a right child" callStack

removeLeftChild :: Tree t -> Tree t
removeLeftChild tree@(Tree _ Nothing _) = tree
removeLeftChild (Tree value left@(Just _) right) = Tree value Nothing right

removeRightChild :: Tree t -> Tree t
removeRightChild tree@(Tree _ _  Nothing) = tree
removeRightChild (Tree value left (Just _)) = Tree value left Nothing