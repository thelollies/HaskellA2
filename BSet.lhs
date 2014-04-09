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
  {-valid,-}
  map) where

import Prelude hiding(foldr, filter, map)
import Data.Foldable

\end{code}

 

\section*{Core}
\begin{code}
data Set a = EmptySet Colour | Node a (Set a) (Set a) Colour
data Colour = B | R | DB | NB

instance Show a => Show (Set a) where
    show a = "{" ++ drop 2 (foldr (\e r -> ", " ++ show e ++ r) "" a) ++ "}"

instance Foldable Set where
  foldr _ z (EmptySet _) = z
  foldr f z (Node k l r _ ) = foldr f (f k (foldr f z r)) l

instance Ord a => Eq (Set a) where
  (EmptySet _) == (EmptySet _) = True
  a == b = (foldr (\e r -> r && (member e b)) True a) && (foldr (\e r -> r && (member e a)) True b)


add :: Ord a => a -> Set a -> Set a
add e s = toB (addHelper e s)

addHelper :: Ord a => a -> Set a -> Set a
addHelper a (EmptySet _) = Node a (EmptySet B) (EmptySet B) R
addHelper a (Node k l r c) | a < k = fix k (addHelper a l) r c
addHelper a (Node k l r c) | a == k = Node k l r c
addHelper a (Node k l r c) | otherwise = fix k l (addHelper a r) c


fix :: Ord a => a -> Set a -> Set a -> Colour -> Set a
fix e (Node e1 (Node e2 l2 r2 R) r1 R) r B = 
  Node e1 (Node e2 l2 r2 B) (Node e r1 r B) R -- lchild to parent, llchild to B sibling (right rotate)
fix e (Node e2 l2 (Node e1 r2 r1 R) R) r B = 
  Node e1 (Node e2 l2 r2 B) (Node e r1 r B) R -- lchild to B sibling, lrchild to parent
fix e2 l2 (Node e (Node e1 r2 r1 R) r R) B = 
  Node e1 (Node e2 l2 r2 B) (Node e r1 r B) R -- rchild to B sibling, rlchild to parent
fix e2 l2 (Node e1 r2 (Node e r1 r R) R) B = 
  Node e1 (Node e2 l2 r2 B) (Node e r1 r B) R -- rchild to parent, rr child to B sibling (left rotate)
fix e l r c = Node e l r c -- No consecutive Reds thus no fixing required
-- TODO add the new cases which handle BB and NB


fixSet :: Ord a => Set a -> Set a
fixSet (Node e l r c) = fix e l r c
fixSet (EmptySet _) = error "Cannot fix an empty set?"

toB :: Set a -> Set a
toB (Node k l r _) = Node k l r B
toB (EmptySet _) = EmptySet B


remove :: Ord a => a -> Set a -> Set a
remove e s = toB(removeHelper e s)

removeHelper :: Ord a => a -> Set a -> Set a
removeHelper e (Node k l r c) | e == k = Node (getRightChild l) (removeHelper (getRightChild l) l) r c
removeHelper e (Node k l r c) | e < k = bubble k (removeHelper e l) r c -- Keep recursing down the left child
removeHelper e (Node k l r c) | e > k = bubble k l (removeHelper e r) c -- keep recursing down the right child
removeHelper e s = s

{- element of node -> node l -> node r -> node colour -> result set -}
bubble :: Ord a => a -> Set a -> Set a -> Colour -> Set a
bubble v l@(Node _ _ _ DB) r c = fixSet (cmp v (subtractBSet l) (subtractBSet r) (addB c))
bubble v l r c = cmp v l r c


cmp :: Ord a => a -> Set a -> Set a -> Colour -> Set a
cmp _ _ _ _ = EmptySet B


rmove :: Ord a => Set a -> Set a
rmove (Node _ (Node _ _ _ _) (Node _ _ _ _) c) = 
rmove _ EmptySet EmptySet R = EmptySet B -- Delete R node with no children
rmove _ EmptySet EmptySet B = EmptySet DB -- Delete B node with no children
rmove (Node _ lNode@(Node e l r Red) _ Black) = toB lNode -- delete B node with one (R) child
rmove (Node _ _ rNode@(Node e l r Red) B) = toB rNode -- delete B node with one (R) child
-- TODO add the killing of nodes with two trees


{-removeHelper e EmptySet = EmptySet
removeHelper e root@(Node k EmptySet EmptySet _) | e == k = EmptySet -- trying to delete where root /= element
                                                 | otherwise = root -- remove root
removeHelper e (Node k (Node k1 l2 r2 c2) EmptySet)
removeHelper e (Node k EmptySet (Node k1 l2 r2 c2)) 
removeHelper e (Node k l r c) | e == k = Node (getRightChild l) (removeHelper (getRightChild l) l) r c
removeHelper e (Node k l r c) | e < k = fix k (removeHelper e l) r c -- Keep recursing down the left child
removeHelper e (Node k l r c) | e > k = fix k l (removeHelper e r) c -- keep recursing down the right child-}

addBSet :: Set a -> Set a
addBSet (EmptySet c) = EmptySet (addB c)
addBSet (Node e l r c) = Node e l r (addB c)

subtractBSet :: Set a -> Set a
subtractBSet (EmptySet c) = EmptySet (subtractB c)
subtractBSet (Node e l r c) = Node e l r (subtractB c)

{- Defining arithmetic for colours -}
addB :: Colour -> Colour
addB DB = error "Cannot add black to double black"
addB B = DB
addB R = B
addB NB = R

subtractB :: Colour -> Colour
subtractB DB = B
subtractB B = R
subtractB R = NB
subtractB NB = error "Cannot subtract black from negative black"

getRightChild :: Ord a => Set a -> a
getRightChild (Node b _ (EmptySet _) _) = b
getRightChild (Node _ _ d _) = getRightChild d
getRightChild (EmptySet _) = error "Cannot get right child of an empty node"


isEmpty :: Set a -> Bool
isEmpty (EmptySet _) = True
isEmpty _ = False


empty :: Set a
empty = (EmptySet B)


singleton :: Ord a => a -> Set a
singleton a = Node a (EmptySet B) (EmptySet B) B


member :: Ord a => a -> Set a -> Bool
member _ (EmptySet _) = False
member a (Node b _ _ _) | a == b = True
member a (Node b c _ _) | a < b = member a c
member a (Node b _ d _) | a > b = member a d
                        | otherwise = False


size :: Set a -> Int
size (EmptySet _) = 0
size (Node _ b c _) = 1 + size b + size c


isSubsetOf :: Ord a => Set a -> Set a -> Bool 
isSubsetOf s1 = foldr (\e r -> r && (member e s1)) True


union :: Ord a => Set a -> Set a -> Set a
union = foldr (\e r -> add e r)


intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 = foldr (\e r -> addIf (\ el -> member el s1 ) e r) (EmptySet B)


difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldr (\e r -> addIf (\ el -> False == (member el s2)) e r) (EmptySet B) s1


filter :: Ord a => (a -> Bool) -> Set a -> Set a
filter f = foldr (\ e r -> addIf f e r) (EmptySet B)

addIf :: Ord a => (a -> Bool) -> a -> Set a -> Set a
addIf f a r | f a = add a r
                | otherwise = r

{-
valid :: Ord a => Set a -> Bool
valid EmptySet = True
valid (Node _ EmptySet EmptySet) = True
valid (Node a (Node b c d) EmptySet) = compare a b == GT && valid (Node b c d)
valid (Node a EmptySet (Node b c d)) = compare a b == LT && valid (Node b c d)
valid (Node a (Node b c d) (Node e f g)) = compare a b == GT && compare a e == LT && 
                                             valid (Node b c d) && valid (Node e f g)
-}

map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
map f = foldr (\ e r -> add (f e) r) (EmptySet B)


\end{code}
\noindent \texttt
\end{code}
\end{document}