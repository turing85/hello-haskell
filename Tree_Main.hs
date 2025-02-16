import Tree (
    Tree (Tree),
    TreeException,
    depth,
    left,
    makeTree,
    removeLeftChild,
    removeRightChild,
    right,
    setLeftChild,
    setRightChild,
    toListInfix,
    toListPrefix,
    toListSuffix,
    value)
import Tree.Ord (isSorted, sortTree)
import Control.Exception (Exception, catch, throw)
import Data.Function ((&))
import GHC.Internal.Stack (HasCallStack, CallStack, callStack, prettyCallStack)
import Text.Printf (printf)
import Distribution.Compat.Prelude (isNothing, Foldable (toList))
import Data.Maybe (Maybe(Nothing))

main :: IO ()
main = do
    let smallTree :: Tree Double = makeSmallTree
    printMessageLn "small tree:" . show $ smallTree
    printMessageLn "small tree left:" . show $ smallTree & left
    printMessageLn "small tree left left:". show $ smallTree & left >>= left
    printMessageLn "small tree left left left:". show $ smallTree & left >>= left >>= left
    printMessageLn "small tree left left right:" . show $ smallTree & left >>= left >>= right
    printMessageLn "small tree right:" . show $ smallTree & right
    printMessageLn "small tree right left:" . show $ smallTree & right >>= left
    printMessageLn "small tree right right:" . show $ smallTree & right >>= right
    printMessageLn "small tree right right right:" . show $ smallTree & right >>= right >>= right
    print $ sum smallTree
    print $ foldr (/) 10 smallTree  -- = 5 / (3 / (-1 / (2 / 10))) = -8.3...
    print $ foldl' (/) 10 smallTree -- = 2 / (3 / (-1 / (10 / 5))) = -0.3...
    print . removeLeftChild . removeRightChild $ smallTree
    catch
        (print .  setRightChild smallTree $ makeTree(-100))
        (printHandler :: TreeException -> IO ())
    catch
        (print . removeLeftChild . removeRightChild . removeLeftChild $ smallTree)
        (printHandler :: TreeException -> IO ())
    let sortedTree :: Tree Double = sortTree smallTree
    printMessageLn "sorted small tree:" . show $ sortedTree
    printMessageLn "sorted small tree left:" . show $ sortedTree
    printMessageLn "sorted small tree left left:" . show $ sortedTree & left >>= left
    printMessageLn
        "sorted small tree left left right:" . show $ sortedTree & left >>= left >>= right
    printMessageLn "one-node sorted tree:" . show . sortTree . makeTree $ 5
    printMessageLn "small tree as list:" . show . toListPrefix $ smallTree
    printMessageLn "sorted small  tree as list:" . show . toListPrefix $ sortedTree
    let largerTree :: Tree Double = makeLargerTree
    printMessageLn "larger tree:" . show $ largerTree
    let sortedLargerTree = sortTree largerTree
    printMessageLn "sorted larger tree:" . show $ sortedLargerTree
    printMessageLn "larger tree as list:" . show . toListPrefix $ largerTree
    printMessageLn "sorted larger tree as list:" . show . toListPrefix $ sortedLargerTree
    printMessageLn "sorted larger tree as infix list:" . show . toListInfix $ sortedLargerTree
    printMessageLn
        "sorted larger tree as suffix list:" . show . toListSuffix $ sortedLargerTree
    printMessageLn "sorted lager tree depth" . show . depth $ largerTree
    printMessageLn "is small tree sorted?" . boolToYesNo . isSorted $ smallTree
    printMessageLn "is sorted small tree sorted?" . boolToYesNo . isSorted $ sortedTree
    printMessageLn "is larger tree sorted?" . boolToYesNo . isSorted $ largerTree
    printMessageLn "is sorted larger tree sorted?" . boolToYesNo . isSorted $ sortedLargerTree
    printMessageLn "is single-node tree sorted?" . boolToYesNo . isSorted . makeTree $ 5
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

printMessageLn :: String -> String -> IO ()
printMessageLn = printf "%-35s %s\n"

boolToYesNo :: Bool -> String
boolToYesNo bool = if bool then "yes" else "no"