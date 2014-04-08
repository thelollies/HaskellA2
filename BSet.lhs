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

{-instance Ord a => Eq (Set a) where
  EmptySet == EmptySet = True
  a == b = (foldr (\e r -> r && (member e b)) True a) && (foldr (\e r -> r && (member e a)) True b)
-}

add :: Ord a => a -> Set a -> Set a
add a (Node b EmptySet c d) | compare a b == LT = Node b (Node a EmptySet EmptySet Red) c d
add a (Node b c EmptySet d) | compare a b == GT = Node b c (Node a EmptySet EmptySet Red) d
add a root@(Node b _ _ _) | compare a b == EQ = root
add a root@(Node k l r c) | compare a k == LT = Node k (addInner a root EmptySet l) r c
add a root@(Node k l r c) | compare a k == GT = Node k l (addInner a root EmptySet r) c
add a set = addInner a EmptySet EmptySet EmptySet 

{- element -> parent -> grandparent -> node -> result -}
addInner :: Ord a => a -> Set a -> Set a -> Set a -> Set a
addInner k EmptySet EmptySet EmptySet = Node k EmptySet EmptySet Black -- Empty set
addInner k _ _ node@(Node k1 _ _ _) | compare k k1 == EQ = node -- Duplicate, return the tree unchanged
addInner k p _ node@(Node k1 l _ _) | compare k k1 == LT = addInner k node p l -- Recurse left
addInner k p _ node@(Node k1 _ r _) | compare k k1 == GT = addInner k node p r -- Recurse right
addInner k (Node pk pl pr Black) _ EmptySet | compare k pk == LT = 
          Node pk (Node k EmptySet EmptySet Red) pr Black-- parent is black and element belongs to parent's left
addInner k (Node pk pl pr Black) _ EmptySet | otherwise =
          Node pk pl (Node k EmptySet EmptySet Red) Black-- parent is black and element belongs to parent's right


-- Now need to deal with the case where I'm adding and the current node is emptyset


{-add a (Node b c d e) | compare a b == EQ = Node b c d e
                   | compare a b == LT = Node b (add a c) d
                   | otherwise = Node b c (add a d)



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


member :: Ord a => a -> Set a -> Bool
member _ (EmptySet) = False
member a (Node b _ _ _) | compare a b == EQ = True
member a (Node b c _ _) | compare a b == LT = member a c
member a (Node b _ d _) | compare a b == GT = member a d
                      | otherwise = False


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