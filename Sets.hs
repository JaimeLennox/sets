module Sets where

import qualified Data.List as List

data Sets a = Elem Int | Set [Sets Int] | ProductSet [(Sets Int, Sets Int)]
       deriving (Eq, Ord)  

instance Show (Sets a) where
  show s
    = show' s
    where
      show'' s = "{" ++ (foldr1 f (map show s)) ++ "}"
      f a b = a ++ ", " ++ b

      show' (Set s)        = show'' s
      show' (Elem s)       = show s
      show' (ProductSet s) = show'' s

-- Example sets to play around with:
-----------------------------------------------------------

a :: Sets Int
a = Set [Set [Elem 1], Set [Elem 2]]

b :: Sets Int
b = Set [Elem 1, Elem 2, Set [Elem 1]]

c :: Sets Int
c = Set [Elem 1, Set [Elem 1]]

d :: Sets Int
d = Set [Elem 1, Elem 2, Elem 3, Elem 4, Elem 5, Elem 6]

x :: Sets Int
x = Set [Elem 1]

x' :: Sets Int
x' = Set [Elem 1, Set [Elem 1]]

ss :: Sets Int
ss = Set [Elem 1, Elem 2, Set [Elem (-1), Elem (-2)], Elem 3]

-- A useful set
empty :: Sets Int
empty = Set []

-----------------------------------------------------------

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
-- Creates n! + 1 subsets
powerSet s = buildSet $ powerSet' s $ cardinality s 
   where 
     powerSet' :: Sets Int -> Int -> [Sets Int]
     powerSet' s@(Set s') n
       | s' == []           = []
       | cardinality s < n  = []
       | n < 1              = []
       | cardinality s == n = s' ++ powerSet' s (n - 1)
       | otherwise          = powerSet' s (n - 1) ++ (map buildSet $ concat
         [List.permutations (s' List.\\ [s' !! y])| y <- [0..(n-1)]])

buildSet :: [Sets Int] -> Sets Int 
buildSet xs = Set (List.sort xs)
