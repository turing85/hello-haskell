import Data.List (sort)
import Text.Printf (printf)

main :: IO()
main = do
    listSamples
    monadSamples
    persons

listSamples :: IO()
listSamples = do
    print . mySort . myReverse $ [1..10]

myReverse :: [t] -> [t]
myReverse [] = []
myReverse(t:ts) = myReverse ts ++ [t]

mySort :: (Ord t) => [t] -> [t]
mySort [] = []
mySort list@(pivot:rest) =
        mySort (filter (< pivot) list) ++
        filter (== pivot) list ++
        mySort(filter (> pivot) list)


monadSamples :: IO()
monadSamples = do
    print . orElseDefault $ (*2) . (+3) <$> safeDivide 1 0
    print . orElseDefault $ (*2) . (+3) <$> safeDivide 1 2

data Failable t = Failure | OK t
    deriving (Eq, Show)
instance Functor Failable where
    fmap :: (a -> b) -> Failable a -> Failable b
    fmap _ Failure = Failure
    fmap f (OK value) = OK (f value)
instance Applicative Failable where
    pure :: a -> Failable a
    pure = OK

    (<*>) :: Failable (a -> b) -> Failable a -> Failable b
    Failure <*> _ = Failure
    _ <*> Failure = Failure
    OK f <*> value = fmap f value
instance Monad Failable where
    (>>=) :: Failable a -> (a -> Failable b) -> Failable b
    Failure >>= _ = Failure
    OK value >>= f = f value

safeDivide :: (Eq t, Fractional t) => t -> t -> Failable t
safeDivide _ 0 = Failure
safeDivide dividend divisor = OK (dividend / divisor)

orElseDefault :: Num n => Failable n -> n
orElseDefault failable = orElse failable (-1)

orElse :: Failable t -> t -> t
orElse (OK value) _ = value
orElse _ value = value

persons :: IO()
persons = do
    let john = Person "John" "Doe" 42
    let johnny = Person "John" "Doe" 43
    let jd = Person "John" "Doe" 42
    let jack = Person "Jack" "Doe" 42

    putStrLn $ "John:   " ++ show john
    putStrLn $ "Johnny: " ++ show johnny
    putStrLn $ "jd:     " ++ show jd
    putStrLn $ "Jack:   " ++ show jack

    putStrLn $ "john == johnny? " ++ show (john == johnny)
    putStrLn $ "john == jd?     " ++ show (john == jd)
    putStrLn $ "john == jack?   " ++ show (john == jack)

    let persons = [john, johnny, jd, jack]
    putStrLn $ "unsorted persons: " ++ show persons
    putStrLn $ "sorted persons:   " ++ (show . sort $ persons)
    putStrLn $ "mySorted persons: " ++ (show . mySort $ persons)

type FirstName = String
type LastName = String
type Age = Integer
data Person = Person { firstName :: FirstName, lastName :: LastName, age :: Age }
  deriving (Eq, Ord)
instance Show Person where
    show :: Person -> String
    show person = printf
        "{\"name\":\"%s %s\",\"age\": %d}"
        (firstName person)
        (lastName person)
        (age person)
-- This is equivalent to the methods generated by "deriving (Eq, Ord)"
-- instance Eq Person where
--     (==) :: Person -> Person -> Bool
--     (==) lhs rhs =
--         firstName lhs == firstName rhs &&
--         lastName lhs == lastName rhs &&
--         age lhs == age rhs

--     (/=) :: Person -> Person -> Bool
--     (/=) lhs rhs =
--         firstName lhs /= firstName rhs ||
--         lastName lhs /= lastName rhs ||
--         age lhs /= age rhs
-- instance Ord Person where
--     (<) :: Person -> Person -> Bool
--     lhs < rhs
--         | compareFirstNames == LT = True
--         | compareFirstNames == EQ =
--             compareLastNames == LT  || (compareLastNames == EQ  && compareAges == LT)
--         | compareFirstNames == GT = False
--         where
--             compareFirstNames = firstName lhs `compare` firstName rhs
--             compareLastNames =  lastName lhs `compare` lastName rhs
--             compareAges = age lhs `compare` age rhs

--     (<=) :: Person -> Person -> Bool
--     lhs <= rhs = lhs < lhs || lhs == rhs

--     (>) :: Person -> Person -> Bool
--     lhs > rhs = rhs < lhs

--     (>=) :: Person -> Person -> Bool
--     lhs >= rhs = rhs < lhs || rhs == lhs
--     compare lhs rhs
--         | lhs == rhs = EQ
--         | lhs < rhs = LT
--         | lhs > rhs = GT