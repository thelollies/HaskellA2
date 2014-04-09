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
module Set (
  Set, 
  add,
  remove,
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
  map) where

import Prelude hiding(foldr, filter, map)
import Data.Foldable

\end{code}



\section*{Core}
\begin{code}
data Set a = EmptySet | Node a (Set a) (Set a)


instance Show a => Show (Set a) where
    show a = "{" ++ drop 2 (foldr (\e r -> ", " ++ show e ++ r) "" a) ++ "}"

instance Foldable Set where
  foldr _ z EmptySet = z
  foldr f z (Node k l r) = foldr f (f k (foldr f z r)) l

instance Ord a => Eq (Set a) where
  EmptySet == EmptySet = True
  a == b = (foldr (\e r -> r && (member e b)) True a) && (foldr (\e r -> r && (member e a)) True b)

add :: Ord a => a -> Set a -> Set a
add a EmptySet = Node a (EmptySet) (EmptySet)
add a (Node b c d) | compare a b == EQ = Node b c d
                   | compare a b == LT = Node b (add a c) d
                   | otherwise = Node b c (add a d)


remove :: Ord a => a -> Set a -> Set a
remove _ EmptySet = EmptySet
remove a n@(Node b EmptySet EmptySet) | a == b = EmptySet
                                      | otherwise = n
remove a (Node b EmptySet d) | a == b = d
remove a (Node b c EmptySet) | a == b = c
remove a (Node b c d) | a < b = Node b (remove a c) d
                      | a > b = Node b c (remove a d)
                      | otherwise = Node (getRightChild c) (remove (getRightChild c) c) d


getRightChild :: Ord a => Set a -> a
getRightChild (Node b _ EmptySet) = b
getRightChild (Node _ _ d) = getRightChild d
getRightChild EmptySet = error "Cannot get right child of an empty node"


isEmpty :: Set a -> Bool
isEmpty (EmptySet) = True
isEmpty _ = False


empty :: Set a
empty = EmptySet


singleton :: Ord a => a -> Set a
singleton a = Node a (EmptySet) (EmptySet)


member :: Ord a => a -> Set a -> Bool
member _ (EmptySet) = False
member a (Node b _ _) | compare a b == EQ = True
member a (Node b c _) | compare a b == LT = member a c
member a (Node b _ d) | compare a b == GT = member a d
                      | otherwise = False


size :: Set a -> Int
size (EmptySet) = 0
size (Node _ b c) = 1 + size b + size c


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
valid (Node a (Node b c d) EmptySet) = a > b && valid (Node b c d) && (not (member a c || member a d))
valid (Node a EmptySet (Node b c d)) = a < b && valid (Node b c d) && (not (member a c || member a d))
valid (Node a l@(Node b c d) r@(Node e f g)) = a > b && a < e 
                                            && valid (Node b c d) -- The left child is correctly arranged
                                            && valid (Node e f g) -- The right child is correctly arranged
                                            && not (member a l) -- Element is not contained in left side
                                            && not (member a r) -- Element is not contained in right side

map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
map f = foldr (\ e r -> add (f e) r) EmptySet

\end{code}
\noindent \texttt
\end{code}
\end{document}