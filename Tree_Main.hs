import Tree (
    TreeException,
    Tree,
    value,
    left,
    right,
    makeTree,
    sortTree,
    toListInfix,
    toListSuffix,
    setLeftChild,
    setRightChild,
    removeLeftChild,
    removeRightChild)
import Control.Exception (Exception, catch, throw)
import Data.Function ((&))
import GHC.Internal.Stack (HasCallStack, CallStack, callStack, prettyCallStack)
import Text.Printf (printf)
import Distribution.Compat.Prelude (isNothing, Foldable (toList))

main :: IO ()
main = do
    let myTree :: Tree Double = makeSmallTree
    printf "tree:                               %s\n" . show $ myTree
    printf "left:                               %s\n" . show $ myTree & left
    printf "left leftx:                         %s\n" . show $ myTree & left >>= left
    printf "left left left:                     %s\n" . show $ myTree & left >>= left >>= left
    printf "left left right:                    %s\n" . show $ myTree & left >>= left >>= right
    printf "right:                              %s\n" . show $ myTree & right
    printf "right left:                         %s\n" . show $ myTree & right >>= left
    printf "right right:                        %s\n" . show $ myTree & right >>= right
    printf "right right right:                  %s\n" . show $ myTree & right >>= right >>= right
    print $ sum myTree
    print $ foldr (/) 10 myTree  -- = 5 / (3 / (-1 / (2 / 10))) = -8.3...
    print $ foldl' (/) 10 myTree -- = 2 / (3 / (-1 / (10 / 5))) = -0.3...
    print . removeLeftChild . removeRightChild $ myTree
    catch (print .  setRightChild myTree $ makeTree(-100)) (printHandler :: TreeException -> IO ())
    catch (print . removeLeftChild . removeRightChild . removeLeftChild $ myTree) (printHandler :: TreeException -> IO ())
    let sortedTree :: Tree Double = sortTree myTree
    printf "sorted tree:                        %s\n" . show $ sortedTree
    printf "sorted tree left:                   %s\n" . show $ sortedTree
    printf "sorted tree left left:              %s\n" . show $ sortedTree & left >>= left
    printf "sorted tree left left right:        %s\n" . show $ sortedTree & left >>= left >>= right
    printf "one-node sorted tree:               %s\n" . show . sortTree . makeTree $ 5
    printf "tree as list:                       %s\n" . show . toList $ myTree
    printf "sorted tree as list:                %s\n" . show . toList . sortTree $ myTree
    let largerTree :: Tree Double = makeLargerTree
    printf "larger tree:                        %s\n" . show $ largerTree
    printf "larger sorted tree:                 %s\n" . show . sortTree $ largerTree
    printf "larger tree as list:                %s\n" . show . toList $ largerTree
    printf "larger sorted tree as list:         %s\n" . show . toList . sortTree $ largerTree
    printf "larger sorted tree as infix list:   %s\n" . show . toListInfix . sortTree $ largerTree
    printf "larger sorted tree as suffix list:  %s\n" . show . toListSuffix . sortTree $ largerTree
    where
        printHandler :: (Exception t) => t -> IO ()
        printHandler = print

makeSmallTree :: Tree Double
makeSmallTree = makeTree 5
    `setLeftChild`
        (makeTree 3
            `setLeftChild` makeTree (-1))
    `setRightChild` makeTree 2

makeLargerTree :: Tree Double
makeLargerTree = makeTree 4
    `setLeftChild`
        (makeTree 6
            `setLeftChild` makeTree 7
            `setRightChild` makeTree 5)
    `setRightChild`
        (makeTree 2
            `setLeftChild` makeTree 3
            `setRightChild` makeTree 1)
