import Prelude ()
import MiniPrelude

sum' :: List Int -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

map' :: (a -> b) -> List a -> List b
map' f [] = []
map' f (x:xs) = f x : map' f xs

(+++) :: List a -> List a -> List a
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

length' :: List a -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

concat' :: List (List a) -> List a
concat' [] = []
concat' (xs: xss) = xs ++ concat' xss

filter' :: (a -> Bool) -> List a -> List a
filter' p [] = []
filter' p (x:xs) = 
  case p x of
    True -> x: filter' p xs
    False -> filter' p xs

take' :: Int -> List a -> List a
take' 0 xs = []
take' n [] = []
take' n (x:xs) = x: take' (n-1) xs

drop' :: Int -> List a -> List a
drop' 0 xs = xs
drop' n [] = []
drop' n (x: xs) = drop' (n-1) xs

takeWhile' :: (a -> Bool) -> List a -> List a
takeWhile' p [] = []
takeWhile' p (x: xs) =
  case p x of
    True -> x: takeWhile' p xs
    False -> []

inits :: List a -> List (List a)
inits [] = [[]]
inits (x: xs) = []: map (\y -> x:y) (inits xs)
-- (\y -> x:y) == (x:)

dropWhile' :: (a -> Bool) -> List a -> List a
dropWhile' p [] = []
dropWhile' p (x: xs) = 
  case p x of
    True -> dropWhile' p xs
    False -> x: xs

reverse' :: List a -> List a
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

-- tails [1, 2 ,3] = [[1, 2, 3], [2, 3], [3], []]
tails' :: List a -> List (List a)
tails' [] = [[]]
tails' (x: xs) = [x: xs] ++ tails' xs

fib :: Nat -> Nat
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

merge :: List Int -> List Int -> List Int
merge [] [] = []
merge (x:xs) [] = x:xs
merge [] (y:ys) = y:ys
merge (x:xs) (y:ys) = case x <= y of
  True -> x : merge xs (y:ys)
  False -> y : merge (x:xs) ys 

zip' :: List a -> List b -> List (a, b)
zip' [] [] = []
zip' (x:xs) [] = []
zip' [] (y:ys) = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

msort :: List Int -> List Int
msort [] = []
msort [x] = [x]
msort xs = merge (msort first) (msort second)
           where first = fst (splitAt middle xs)
                 second = snd (splitAt middle xs)
                 middle = (length xs) `div` 2