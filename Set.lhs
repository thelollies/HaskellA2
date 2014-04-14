\documentclass[a4paper,12pt]{article}
\usepackage{minted}
\title{Assignment 2 - BST Set Implementation}
\author{Rory Stephenson 300160212 stepherory}
\date{14 April 2014}
\begin{document}
\newminted[code]{haskell}{linenos}
\maketitle

\section*{Module Setup}

Internal helper functions are not exported, nor are the constructors of \texttt{Set}. Without the constructors the only way to obtain or modify a \texttt{Set} is via the exported functions which means that as long as these maintain the validity of the tree structure, an invalid \texttt{Set} cannot be obtained. This obviously excludes the case where the module is imported into ghci, ignoring what the module hides.

\texttt{Prelude} is explicitly imported in order to hide its implementation of \texttt{foldr}, \texttt{filter} and \texttt{map} (which collide with functions in this module). \texttt{Data.Foldable} is used to implement folds on the \texttt{Set}.

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

\section*{Data and Instance declarations}

\texttt{Set} is declared with two constructors, \texttt{EmptySet} and \texttt{Node}, which give it a tree structure. \texttt{EmptySet} acts as a leaf and is also used to represent a set with no elements in it. \texttt{Node} provides node structure in the tree, it holds an element as well as a left and right \texttt{Set}. The use of \texttt{a} in the set declaration ensures that the set can only hold one type.

The three instance declarations implement \texttt{Show}, \texttt{Foldable} and \texttt{Eq}. \texttt{Show} is used to render the set as a string in the format \{e$_{1}$, e$_{2}$ , .., e$_{n}$\} where the elements are ordered. Whilst sets do not have order, the ordering reflects the structure of the underlying tree. \texttt{Foldable} allows \texttt{Set} to be folded on and also acts in order (to provide some predictability for someone folding on a \texttt{Set} with a function like (-) where order matters). The drawback of this method is that if a fold is used to add to a set, the resulting set may have the performance of a linked list because the elements will be added in order.  Finally \texttt{Eq} checks equality of two sets by checking whether each set contains all of the elements of the other set. 

\begin{code}
data Set a = EmptySet | Node a (Set a) (Set a)


instance Show a => Show (Set a) where
    show a = "{" ++ drop 2 (foldr (\e r -> ", " ++ show e ++ r) "" a) ++ "}"

instance Foldable Set where
  foldr _ z EmptySet = z
  foldr f z (Node k l r) = foldr f (f k (foldr f z r)) l

instance Ord a => Eq (Set a) where
  EmptySet == EmptySet = True
  a == b = (foldr (\e r -> r && (member e b)) True a) && 
           (foldr (\e r -> r && (member e a)) True b)

\end{code}

\noindent \texttt{add} is a typical BST insert except it will not add the element if it is a duplicate (since this tree represents a set), it instead returns the tree unmodified. \texttt{add} recurses down the left/right branches of the tree if the new element is less/greater than the current node and returns a new node with no children once the appropriate position is found. Elements to be added must be an instance of \texttt{Ord} because finding the correct location of an element in the tree requires comparison.

\begin{code}
add :: Ord a => a -> Set a -> Set a
add a EmptySet = Node a (EmptySet) (EmptySet)
add a (Node b c d) | a == b = Node b c d
                   | a < b = Node b (add a c) d
                   | otherwise = Node b c (add a d)
\end{code}

\noindent \texttt{remove} recurses down the tree until the specified element is found. If the element is in a node with one child it replaces the element with its child, otherwise it removes the node's in-order predecessor (found by calling \texttt{getRightChild} on the left child of the node) and replaces the node with the removed child. \texttt{Ord} is again requried in order to find the target node in the tree via comparisons.
 
\begin{code}
remove :: Ord a => a -> Set a -> Set a
remove _ EmptySet = EmptySet
remove a n@(Node b EmptySet EmptySet) | a == b = EmptySet
                                      | otherwise = n
remove a (Node b EmptySet d) | a == b = d
remove a (Node b c EmptySet) | a == b = c
remove a (Node b c d) | a < b = Node b (remove a c) d
                      | a > b = Node b c (remove a d)
                      | otherwise = 
                        Node (getRightChild c) (remove (getRightChild c) c) d
\end{code}

\noindent \texttt{getRightChild} recursively descends down the right branch of a node until it reaches the rightmost child and returns it. An error is thrown if \texttt{getRightChild} is called on \texttt{EmptySet} because a leaf has no children and no logical element could be returned which could be considered the right child of a leaf. The element also couldn't satisfy the function's type declaration, which requires an element of the same type of that which is held in the provided set to be returned, since the set holds no elements. This error should never occur during runtime because the function is only used in \texttt{remove} and it is called on the left child of a \texttt{Node} which has two children.
 
\begin{code}
getRightChild :: Set a -> a
getRightChild (Node b _ EmptySet) = b
getRightChild (Node _ _ d) = getRightChild d
getRightChild EmptySet = error "Cannot find right child of an empty set."
\end{code}

\noindent \texttt{isEmpty} simply returns \texttt{True} if the provided set is the \texttt{EmptySet} or \texttt{False} otherwise.
 
\begin{code}
isEmpty :: Set a -> Bool
isEmpty EmptySet = True
isEmpty _ = False
\end{code}

\noindent \texttt{empty} returns the \texttt{EmptySet}. \texttt{Ord} is not enforced here both because it is not required by the function and because it will be enforced by \texttt{add} if anything is added to the resulting empty \texttt{Set}. Having an order in an empty tree structure is meaningless.

\begin{code}
empty :: Set a
empty = EmptySet
\end{code}

\noindent \texttt{singleton} returns a set containing only the specified element. Once again \texttt{Ord} is not enforced since it will be enforced by other functions if anything is added or removed. There is also no need for ordering in a tree that only has a root.

\begin{code}
singleton :: a -> Set a
singleton a = Node a (EmptySet) (EmptySet)
\end{code}

\noindent \texttt{member} returns \texttt{True} when the given element is in the given \texttt{Set} or \texttt{False} otherwise. It recurses in a similar fashion to \texttt{add}, returning \texttt{True} if it finds an element which is equal to the provided one or \texttt{False} if it reaches \texttt{EmptySet}. \texttt{Ord} is needed for the tree navigation and to check if the desired element has been found.

\begin{code}
member :: Ord a => a -> Set a -> Bool
member _ EmptySet = False
member a (Node b _ _) | a == b = True
member a (Node b c _) | a < b = member a c
member a (Node b _ d) | a > b = member a d
                      | otherwise = False
\end{code}

\noindent \texttt{size} calculates the number of elements in the \texttt{Set} by counting how many nodes the set has (since leaves cannot hold elements).

\begin{code}
size :: Set a -> Int
size (EmptySet) = 0
size (Node _ b c) = 1 + size b + size c
\end{code}

\noindent \texttt{isSubsetOf} returns \texttt{True} if all the elements of the first set are contained in the second set (opposite to the assignment brief because it makes more sense when the function is called infix). \texttt{Ord} is required since member is called to check if it is a subset.

\begin{code}
isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf EmptySet _ =  True
isSubsetOf s1 s2 = foldr (\e r -> r && (member e s2)) True s1
\end{code}

\noindent \texttt{union} returns a set containing all the elements in both sets (no duplicates). Using a fold to add the second set to the first means that the resulting set may not perform well if the two sets' elements are not of a similar range because all of the added elements will form a linked list beginning from the closest element in the other set. \texttt{Ord} is required because \texttt{add} is called inside this function on the provided sets.

\begin{code}
union :: Ord a => Set a -> Set a -> Set a
union = foldr add
\end{code}

\noindent \texttt{intersection} creates a \texttt{Set} containing all the elements which are in both of the provided sets. It suffers from the same issue described in \texttt{union} but to an even greater degree because it adds all qualifying elements to a new empty \texttt{Set}, in order.

\begin{code}
intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 = foldr (\e r -> addIf (\ el -> member el s1 ) e r) EmptySet
\end{code}

\noindent \texttt{difference} returns a set containing the elements of the first \texttt{Set} which are not contained in the second \texttt{Set}. It suffers from the same issue as \texttt{intersection}.

\begin{code}
difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = 
  foldr (\e r -> addIf (\ el -> not $  (member el s2)) e r) EmptySet s1
\end{code}

\noindent \texttt{filter} returns a \texttt{Set} containing all the elements which evaluate to \texttt{True} when the given function is applied to them. It suffers from the same issue as \texttt{intersection}.

\begin{code}
filter :: Ord a => (a -> Bool) -> Set a -> Set a
filter f = foldr (\ e r -> addIf f e r) EmptySet
\end{code}

\noindent \texttt{addif} is an internal helper function which returns the specified set, adding the specified element to that set if the given function returns \texttt{True} when applied to that element.

\begin{code}
addIf :: Ord a => (a -> Bool) -> a -> Set a -> Set a
addIf f a r | f a = add a r
            | otherwise = r
\end{code}

\noindent \texttt{valid} Evaluates whether a \texttt{Set} is valid both in terms of the semantics of sets and the underlying tree structure. Specifically it checks whether:

\begin{enumerate}
  \item There are any duplicates in the set
  \item Every \texttt{Node}'s element is greater than those in its left subtree and less than those in its right subtree.
\end{enumerate}

\begin{code}
valid :: Ord a => Set a -> Bool
valid EmptySet = True
valid (Node _ EmptySet EmptySet) = True
valid (Node a (Node b c d) EmptySet) = a > b && valid (Node b c d) && 
                                     (not (member a c || member a d))
valid (Node a EmptySet (Node b c d)) = a < b && valid (Node b c d) && 
                                     (not (member a c || member a d))
valid (Node a l@(Node b c d) r@(Node e f g)) = a > b && a < e && 
                 valid (Node b c d) && -- The left child is correctly arranged
                 valid (Node e f g) && -- The right child is correctly arranged
                 not (member a l) && -- Element is not contained in left side
                 not (member a r) -- Element is not contained in right side
\end{code}

\noindent \texttt{map} implements mapping over a \texttt{Set}. It suffers from the linked list problem resulting from the use of folding.

\begin{code}
map :: Ord b => (a -> b) -> Set a -> Set b
map f = foldr (\ e r -> add (f e) r) EmptySet
\end{code}
\end{document}