\documentclass[a4paper,12pt]{article}
\usepackage{minted}
\title{Assignment 1}
\author{Rory Stephenson 300160212 stepherory}
\date{6 April 2014}
\begin{document}
\newminted[code]{haskell}{linenos}
\maketitle

\section*{Module Setup}
\begin{code}
module BSet (
  Set, 
  add,
{-  remove,
  isEmpty,
  empty,
  singleton,
  member,
  size,
  isSubsetOf,
  union,
  intersection,
  difference,
  filter,
  valid,
  map-}) where

import Prelude hiding(foldr, filter, map)
import Data.Foldable

\end{code}



\section*{Core}
\begin{code}
data Set a = EmptySet | Node a (Set a) (Set a) Colour
data Colour = Black | Red

instance Show a => Show (Set a) where
    show a = "{" ++ drop 2 (foldr (\e r -> ", " ++ show e ++ r) "" a) ++ "}"

instance Foldable Set where
  foldr _ z EmptySet = z
  foldr f z (Node k l r _ ) = foldr f (f k (foldr f z r)) l

instance Ord a => Eq (Set a) where
  EmptySet == EmptySet = True
  a == b = (foldr (\e r -> r && (member e b)) True a) && (foldr (\e r -> r && (member e a)) True b)


add :: Ord a => a -> Set a -> Set a
add e s = toBlack (addHelper e s)

addHelper :: Ord a => a -> Set a -> Set a
addHelper a EmptySet = Node a EmptySet EmptySet Red
addHelper a (Node k l r c) | a < k = fix k (addHelper a l) r c
addHelper a (Node k l r c) | a == k = Node k l r c
addHelper a (Node k l r c) | a > k = fix k l (addHelper a r) c


fix :: Ord a => a -> Set a -> Set a -> Colour -> Set a
fix e (Node e1 (Node e2 l2 r2 Red) r1 Red) r Black = Node e1 (Node e2 l2 r2 Black) (Node e r1 r Black) Red
fix e (Node e2 l2 (Node e1 r2 r1 Red) Red) r Black = Node e1 (Node e2 l2 r2 Black) (Node e r1 r Black) Red
fix e2 l2 (Node e (Node e1 r2 r1 Red) r Red) Black = Node e1 (Node e2 l2 r2 Black) (Node e r1 r Black) Red
fix e2 l2 (Node e1 r2 (Node e r1 r Red) Red) Black = Node e1 (Node e2 l2 r2 Black) (Node e r1 r Black) Red
fix e l r c = Node e l r c -- No fixing required


toBlack :: Set a -> Set a
toBlack (Node k l r c) = Node k l r Black
toBlack EmptySet = EmptySet


{-
remove :: Ord a => a -> Set a -> Set a
remove _ EmptySet = EmptySet
remove a (Node b EmptySet EmptySet) | compare a b == EQ = EmptySet
                                    | otherwise = singleton b
remove a (Node b EmptySet d) | compare a b == EQ = d
remove a (Node b c EmptySet) | compare a b == EQ = c
remove a (Node b c d) | compare a b == LT = Node b (remove a c) d
                      | compare a b == GT = Node b c (remove a d)
                      | otherwise         = Node (getRightChild c) (remove (getRightChild c) c) d


getRightChild :: Ord a => Set a -> a
getRightChild (Node b _ EmptySet _) = b
getRightChild (Node _ _ d _) = getRightChild d
getRightChild EmptySet = error "Cannot get right child of an empty node"


isEmpty :: Set a -> Bool
isEmpty (EmptySet) = True
isEmpty _ = False


empty :: Set a
empty = EmptySet


singleton :: Ord a => a -> Set a
singleton a = Node a (EmptySet) (EmptySet) Black
-}

member :: Ord a => a -> Set a -> Bool
member _ (EmptySet) = False
member a (Node b _ _ _) | a == b = True
member a (Node b c _ _) | a < b = member a c
member a (Node b _ d _) | a > b = member a d
                        | otherwise = False

{-
size :: Set a -> Int
size (EmptySet) = 0
size (Node _ b c _) = 1 + size b + size c


isSubsetOf :: Ord a => Set a -> Set a -> Bool 
isSubsetOf s1 = foldr (\e r -> r && (member e s1)) True


union :: Ord a => Set a -> Set a -> Set a
union = foldr (\e r -> add e r)


intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 = foldr (\e r -> addIf (\ el -> member el s1 ) e r) EmptySet


difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldr (\e r -> addIf (\ el -> False == (member el s2)) e r) EmptySet s1


filter :: Ord a => (a -> Bool) -> Set a -> Set a
filter f = foldr (\ e r -> addIf f e r) EmptySet

addIf :: Ord a => (a -> Bool) -> a -> Set a -> Set a
addIf f a r | f a = add a r
                | otherwise = r


valid :: Ord a => Set a -> Bool
valid EmptySet = True
valid (Node _ EmptySet EmptySet) = True
valid (Node a (Node b c d) EmptySet) = compare a b == GT && valid (Node b c d)
valid (Node a EmptySet (Node b c d)) = compare a b == LT && valid (Node b c d)
valid (Node a (Node b c d) (Node e f g)) = compare a b == GT && compare a e == LT && 
                                             valid (Node b c d) && valid (Node e f g)

map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
map f = foldr (\ e r -> add (f e) r) EmptySet-}


\end{code}
\noindent \texttt
\end{code}
\end{document}