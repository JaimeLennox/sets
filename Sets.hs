module Sets where

import qualified Data.List as List

data Sets a = SingleSet Int | Set [Sets Int] | ProductSet [(Sets Int, Sets Int)]
       deriving (Eq, Ord)  

instance Show (Sets a) where
  show (Set s) = show s
  show (SingleSet s) = show s
  show (ProductSet s) = show s

{-
a :: Sets Int
a = Set [SingleSet 1, SingleSet 3, SingleSet 5]

b :: Sets Int
b = Set [SingleSet 2, SingleSet 3]

c :: Sets Int
c = Set [SingleSet 1, SingleSet 5]
-}

a :: Sets Int
a = Set [Set [SingleSet 1], Set [SingleSet 2]]

b :: Sets Int
b = Set [SingleSet 1, SingleSet 2, Set [SingleSet 1]]

c :: Sets Int
c = Set [SingleSet 1, Set [SingleSet 1]]

x :: Sets Int
x = Set [SingleSet 1]

x' :: Sets Int
x' = Set [SingleSet 1, Set [SingleSet 1]]

ss :: Sets Int
ss = Set [SingleSet 1, SingleSet 2, Set [SingleSet (-1), SingleSet (-2)], SingleSet 3]

empty :: Sets Int
empty = Set []

union :: Sets Int -> Sets Int -> Sets Int
union (Set s) (Set s') = buildSet $ List.nub $ s ++ s'

intersection :: Sets Int -> Sets Int -> Sets Int
intersection (Set s) (Set s') = buildSet [x | x <- s, elem x s']

diff :: Sets Int -> Sets Int -> Sets Int
diff (Set s) (Set s') = buildSet [ x | x <- s, not (elem x s') ]

symDiff :: Sets Int -> Sets Int -> Sets Int
symDiff s s' = union (diff s s') (diff s' s)

cardinality :: Sets Int -> Int
cardinality (Set s) = length s 

isSubSet :: Sets Int -> Sets Int -> Bool
isSubSet (Set s) (Set s') = s List.\\ s' == [] 

isMember :: Sets Int -> Sets Int -> Bool
isMember s (Set s') = elem s s'

productSet :: Sets Int -> Sets Int -> Sets Int
productSet (Set s) (Set s') = ProductSet [ (x,y) | x <- s, y <- s']

powerSet :: Sets Int -> Sets Int
powerSet (Set s) = Set [] --TODO; will create n! + 1 subsets

buildSet :: [Sets Int] -> Sets Int 
buildSet xs = Set (List.sort xs)
