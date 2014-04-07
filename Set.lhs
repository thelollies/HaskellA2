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
  intersection) where

import Prelude hiding(foldr)
import Data.Foldable

\end{code}



\section*{Core}
\begin{code}
data Set a = EmptySet | Node a (Set a) (Set a)


instance Show a => Show (Set a) where
    show = showSet 0 


instance Foldable Set where
  foldr _ z EmptySet = z
  foldr f z (Node k l r) = foldr f (f k (foldr f z r)) l

instance Ord a => Eq (Set a) where
  EmptySet == EmptySet = True
  a == b = (foldr (\e r -> r && (member e b)) True a) && (foldr (\e r -> r && (member e a)) True b)

showSet :: Show a => Int -> Set a -> String
showSet _ EmptySet = ""
showSet indent (Node a b c) = ("\n" ++ (replicate indent ' ')) ++ 
                              show a ++ 
                              showSet (succ indent) b ++ 
                              showSet (succ indent) c

add :: Ord a => a -> Set a -> Set a
add a EmptySet = Node a (EmptySet) (EmptySet)
add a (Node b c d) | compare a b == EQ = Node b c d
add a (Node b c d) | compare a b == LT = Node b (add a c) d
add a (Node b c d) | otherwise = Node b c (add a d)


remove :: Ord a => a -> Set a -> Set a
remove _ EmptySet = EmptySet
remove a (Node b EmptySet EmptySet) | compare a b == EQ = EmptySet
remove _ (Node b EmptySet EmptySet) | otherwise         = singleton b
remove a (Node b EmptySet d) | compare a b == EQ = d
remove a (Node b c EmptySet) | compare a b == EQ = c
remove a (Node b c d) | compare a b == LT = Node b (remove a c) d
remove a (Node b c d) | compare a b == GT = Node b c (remove a d)
remove _ (Node _ c d) | otherwise         = Node (getRightChild c) (remove (getRightChild c) c) d

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
member _ _ = False


size :: Set a -> Int
size (EmptySet) = 0
size (Node _ b c) = 1 + size b + size c


isSubsetOf :: Ord a => Set a -> Set a -> Bool 
isSubsetOf s1 = foldr (\e r -> r && (member e s1)) True


union :: Ord a => Set a -> Set a -> Set a
union = foldr (\e r -> add e r)

intersection :: Ord a => Set a -> Set a -> Set a
intersection 

\end{code}
\noindent \texttt
\end{code}
\end{document}