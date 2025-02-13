module Weekday (Weekday, isWeekend) where

import Control.Exception (Exception, throw)
import Text.Printf (printf)
import Data.List (sort)

main :: IO()
main = do
    print [1, 3 .. 10]
    printPredecessors $ map (\value -> toEnum value::Weekday) [7..13]
    print "---"
    printSuccessors [Monday .. Sunday]
    print "---"
    printIsWeekday . sort $ [Sunday, Saturday .. Monday]

printPredecessors :: [Weekday] -> IO()
printPredecessors weekdays = do
    mapM_
        (\weekday -> printf
            "The day after %s is %s\n"
            (show weekday)
            (show . succ $ weekday))
        weekdays

printSuccessors :: [Weekday] -> IO()
printSuccessors weekdays = do
    mapM_
        (\weekday -> printf
            "The day before %s is %s\n"
            (show weekday)
            (show . pred $ weekday))
        weekdays

printIsWeekday :: [Weekday] -> IO()
printIsWeekday weekdays = do
    mapM_
        (\weekday -> printf
            "Is %s a workday? %s.%s\n"
            (show weekday)
            (case (isWorkday weekday) of
                True -> "Yes"
                False -> "No")
            (case weekday of
                Wednesday -> " It's Wednesday my Dudes."
                Friday -> " IT'S FRIDAY THEN! GO MUFASA!"
                otherwise -> ""))
        weekdays

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show)
instance Enum Weekday where
    fromEnum :: Weekday -> Int
    fromEnum Monday = 0
    fromEnum Tuesday = 1
    fromEnum Wednesday = 2
    fromEnum Thursday = 3
    fromEnum Friday = 4
    fromEnum Saturday = 5
    fromEnum Sunday = 6

    toEnum :: Int -> Weekday
    toEnum weekday = case (weekday `mod` 7) of
        0 -> Monday
        1 -> Tuesday
        2 -> Wednesday
        3 -> Thursday
        4 -> Friday
        5 -> Saturday
        6 -> Sunday

isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

isWorkday :: Weekday -> Bool
isWorkday weekday = not $ isWeekend weekday