module M7 where
import Prelude ()
import MiniPrelude

f0 :: Eq a => a -> List a -> List (a, a)
f0 (-..-) = (.|--.-|-.+|.|-..-|-) (-..-)
f1 :: List a -> List (a, a)
f1 = (.--|..|-|....|-.+|.|-..-|-)


(..-.|...|-) ((-..-),(-.--)) = (-..-)
(...|-.|-..) ((-..-),(-.--)) = (-.--)
(..-.|---|.-..|-..|.-.) (..-.) (.| ) [] = (.| )
(..-.|---|.-..|-..|.-.) (..-.) (.| ) ((-..-):(-..-|...)) = (..-.) (-..-) ((..-.|---|.-..|-..|.-.) (..-.) (.| ) (-..-|...))
(--|.-|.--.) (..-.) [] = []
(--|.-|.--.) (..-.) ((-..-) : (-..-|...)) = (..-.) (-..-) : (--|.-|.--.) (..-.) (-..-|...)
(--..|..|.--.) [] _ = []
(--..|..|.--.) ((-..-):(-..-|...)) [] = []
(--..|..|.--.) ((-..-):(-..-|...)) ((-.--):(-.--|...)) = ((-..-),(-.--)) : (--..|..|.--.) (-..-|...) (-.--|...)
(..-.|..|.-..|-|.|.-.) (.--.) = (..-.|---|.-..|-..|.-.) (\(-..-) (-..-|...) -> if (.--.) (-..-) then (-..-) : (-..-|...) else (-..-|...)) []
(....|.|.-|-..) ((-..-):(-..-|...)) = (-..-)
(-|.-|..|.-..) ((-..-):(-..-|...)) = (-..-|...)
(.--|..|-|....|-.+|.|-..-|-) (-..-|...) = (--..|..|.--.) (-..-|...) ((-|.-|..|.-..) (-..-|...))
(.|--.-|-.+|.|-..-|-) (-..-) = (..-.|..|.-..|-|.|.-.) (((-..-) ==) . (..-.|...|-)) . (.--|..|-|....|-.+|.|-..-|-)
(-.-.|---|-.|-.-.|.-|-) = (..-.|---|.-..|-..|.-.) (++) []
(.-..|.|-.|--.|-|....) :: [a] -> Int
(.-..|.|-.|--.|-|....) [] = 0
(.-..|.|-.|--.|-|....) ((-..-):(-..-|...)) = 1 + (.-..|.|-.|--.|-|....) (-..-|...)
(-..|.-.|---|.--.) :: Int -> [a] -> [a]
(-..|.-.|---|.--.) 0 (-..-|...) = (-..-|...)
(-..|.-.|---|.--.) _ [] = []
(-..|.-.|---|.--.) (-.) ((-..-):(-..-|...)) = (-..|.-.|---|.--.) ((-.)-1) (-..-|...)
(-|.-|-.-|.) :: Int -> [a] -> [a]
(-|.-|-.-|.) 0 _ = []
(-|.-|-.-|.) _ [] = []
(-|.-|-.-|.) (-.) ((-..-):(-..-|...)) = (-..-) : (-|.-|-.-|.) ((-.)-1) (-..-|...)
(-.-.|---|-.|...|-) (-..-) (-.--) = (-..-)
