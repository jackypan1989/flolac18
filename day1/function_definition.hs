import Prelude ()
import MiniPrelude

square :: Int -> Int
square x = x * x

smaller :: Int -> Int -> Int
smaller x y = if x <= y then x else y

three :: Int -> Int
three x = 3

inf :: Int
inf = inf + 1

myNull :: [a] -> Bool
myNull [] = True
myNull x = False

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

quadAll :: [Int] -> [Int]
quadAll = doubleAll . doubleAll

-- doubleAll' :: [Int] -> [Int]
-- doubleAll' = \x -> map (2*x)