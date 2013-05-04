module Sets where

import qualified Data.List as List
import Control.Exception

data Sets a = Elem Int | Set [Sets Int] | ProductSet [(Sets Int, Sets Int)]
  deriving (Eq, Ord)  

instance Show (Sets a) where
  show s
    = show' s
    where
      show'' s = "{" ++ (foldr1 f (map show s)) ++ "}"
      f a b = a ++ ", " ++ b

      show' (Set [])               = "{}" 
      show' (ProductSet [])        = "{}"
      show' (Set s)                = show'' s
      show' (Elem s)               = show s
      show' (ProductSet s)         = show'' s

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

r :: Sets Int
r = ProductSet [(Elem 1,Elem 2),(Elem 2,Elem 3),(Elem 3,Elem 4),(Elem 4,Elem 1)]

s :: Sets Int
s = ProductSet [(Elem 1,Elem 2),(Elem 2,Elem 1),(Elem 3,Elem 4),(Elem 4,Elem 3)]

x :: Sets Int
x = Set [Elem 1]

x' :: Sets Int
x' = Set [Elem 1, Set [Elem 1]]

ss :: Sets Int
ss = Set [Elem 1, Elem 2, Set [Elem (-1), Elem (-2)], Elem 3]

-- A useful set
empty :: Sets Int
empty = Set []

-- Natural numbers
nat :: Sets Int
nat = Set $ map Elem [0,1..]

-----------------------------------------------------------
-- Argument Standards
--
-- In general, let R be a binary relation over the set A (with members a[1..n])
--
-- ARG1 : 1st arg = R, 2nd arg = A
--
-- ARG2 : 1st arg = R, 2nd arg = A, 3rd arg = a[i] 
-----------------------------------------------------------

union :: Sets Int -> Sets Int -> Sets Int
union (Set s) (Set s') = buildSet $ List.nub $ s ++ s'
union (ProductSet s) (ProductSet s') = ProductSet $ List.nub $ s ++ s'

intersection :: Sets Int -> Sets Int -> Sets Int
intersection (Set s) (Set s') = buildSet [x | x <- s, elem x s']
intersection (ProductSet s) (ProductSet s') = ProductSet [x | x <- s, elem x s']

diff :: Sets Int -> Sets Int -> Sets Int
diff (Set s) (Set s') = buildSet [ x | x <- s, not (elem x s') ]
diff (ProductSet s) (ProductSet s') = ProductSet [x | x <- s, not (elem x s')]

symDiff :: Sets Int -> Sets Int -> Sets Int
symDiff s s' = union (diff s s') (diff s' s)

cardinality :: Sets Int -> Int
cardinality (Set s) = length s 

isSubSet :: Sets Int -> Sets Int -> Bool
isSubSet (Set s) (Set s') = s List.\\ s' == []
isSubSet (ProductSet s) (ProductSet s') = s List.\\ s' == []

isMember :: Sets Int -> Sets Int -> Bool
isMember s (Set s') = elem s s'

productSet :: Sets Int -> Sets Int -> Sets Int
productSet (Set s) (Set s') = ProductSet [ (x,y) | x <- s, y <- s']

powerSet :: Sets Int -> Sets Int
powerSet (Set s) = buildSet $ map buildSet $ powerSet' s
  where powerSet' :: [Sets Int] -> [[Sets Int]] 
        powerSet' [] = [[]]
        powerSet' (x:xs) = pSet ++ map (x:) pSet
          where pSet = powerSet' xs
powerSet (ProductSet s) = buildSet $ map buildSet' $ powerSet' s
  where powerSet' :: [a] -> [[a]] 
        powerSet' [] = [[]]
        powerSet' (x:xs) = pSet ++ map (x:) pSet
          where pSet = powerSet' xs
        buildSet' xs = ProductSet (List.sort xs) 

setID :: Sets Int -> Sets Int
setID (Set s) = ProductSet [ (x,x) | x <- s]

compose :: Sets Int -> Sets Int -> Sets Int
compose (ProductSet s) (ProductSet s')
  = ProductSet [(x, y') | (x, y) <- s, (x', y') <- s', y == x']

-- ARG1
isReflexive :: Sets Int -> Sets Int -> Bool
isReflexive s s' = isSubSet (setID s') s  

isSymmetric :: Sets Int -> Bool
isSymmetric = (==) (inverse s)

isTransitive :: Sets Int -> Bool
isTransitive s = isSubSet (compose s s) s

-- ARG1
isEqual :: Sets Int -> Sets Int -> Bool
isEqual s s' = isReflexive s s' && isSymmetric s && isTransitive s

inverse :: Sets Int -> Sets Int
inverse (ProductSet s) = ProductSet [ (x, x') | (x', x) <- s]

-- ARG1
complement :: Sets Int -> Sets Int -> Sets Int
complement s s' = assert (isSubSet s setSquared) (ProductSet [x | x <- 
  xs, (not (isMember (tupleToSet x) (productSetToSet s)))])
  where
    setSquared@(ProductSet xs) = productSet s' s'

-- ARG2
equivalenceClass :: Sets Int -> Sets Int -> Sets Int -> [Sets Int]
equivalenceClass pSet@(ProductSet s) set@(Set s') s''
  = assert (isEqual pSet set) ( [ x | x <- s', elem (s'', x) s])

-- ARG2
quotientSet :: Sets Int -> Sets Int -> Sets Int -> Sets Int
quotientSet s s' s'' = Set $ equivalenceClass s s' s''

tupleToSet :: (Sets Int, Sets Int) -> Sets Int
tupleToSet (s, s') = Set [s, s']

tuplesToSet :: [(Sets Int, Sets Int)] -> Sets Int
tuplesToSet [] = Set []
tuplesToSet (x:xs) = Set (tuplesToSet' (x:xs))
  where
    tuplesToSet' :: [(Sets Int, Sets Int)] -> [Sets Int]
    tuplesToSet' [] = []
    tuplesToSet' (x:xs) = tupleToSet x : tuplesToSet' xs 

productSetToSet :: Sets Int -> Sets Int
productSetToSet (ProductSet xs) = tuplesToSet xs

setToTuple :: Sets Int -> (Sets Int, Sets Int)
setToTuple (Set [s, s']) = (s, s')

buildSet :: [Sets Int] -> Sets Int 
buildSet xs = Set (List.sort xs)
