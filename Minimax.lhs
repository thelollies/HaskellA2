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
module Minimax(wins, 
  emptyBoard, 
  showBoard, 
  Player(X,O),
  boardNextMoves,
  next) where

data MTree = Node Board Player [MTree] | Win Board Player
instance Ord MTree where
  (Win _ _) `compare` (Win _ _) = EQ
  (Win _ _) `compare` (Node _ _ _) = GT
  (Node _ _ _) `compare` (Win _ _) = LT
  (Node b1 p1 _) `compare` (Node b2 p2 _) = ((heuristicValue p1 b1 - heuristicValue (next p1) b1)) `compare`
                                            ((heuristicValue p2 b2 - heuristicValue (next p2) b2))
instance Eq MTree where
  (Win _ _) == (Win _ _) = True
  (Win _ _) == (Node _ _ _) = False 
  (Node _ _ _) == (Win _ _) = False
  (Node b1 p1 _) == (Node b2 p2 _) = ((heuristicValue p1 b1 - heuristicValue (next p1) b1)) ==
                                            ((heuristicValue p2 b2 - heuristicValue (next p2) b2))

data Player = X | O
type Square = Either Int Player
type Board = [Square]

instance Show Player where
  show X = "X"
  show O = "O"

instance Eq Player where
  X == X = True
  O == O = True
  _ == _ = False

size :: MTree -> Int
size (Win _ _) = 1
size (Node _ _ a) = 1 + foldr (\e r -> r + size e) 0 a

gameSubTree :: Player -> Player -> Board -> MTree
gameSubTree player comp board | (next player) `wins` board = Win board otherP
                                                             where otherP = next player
gameSubTree player comp board = Node board player (mySort (foldr (\ e r -> (gameSubTree (next player) comp e) : r) [] (boardNextMoves player board)))

heuristicValue :: Player -> Board -> Int
heuristicValue p (a:b:c:d:e:f:g:h:i:_) = canWin p a b c + -- top row
                                         canWin p d e f + -- middle row
                                         canWin p g h i + -- bottom row
                                         canWin p a d g + -- left col
                                         canWin p b e h + -- middle col
                                         canWin p c f i + -- right col
                                         canWin p a e i + -- diag from top left
                                         canWin p g e c   -- diag from bottom right
heuristicValue _ _ = error "Incorrect board dimensions"

canWin :: Player -> Square -> Square -> Square -> Int
canWin p a b c = intOf $ pOrE p a && pOrE p b && pOrE p c
                 where intOf True = 1
                       intOf False = 0

pOrE :: Player -> Square -> Bool
pOrE p (Left _) = True
pOrE p (Right r) | p == r = True
pOrE _ _ = False

mySort :: Ord a => [a] -> [a]
mySort [] = []
mySort [a] = [a]
mySort xs = merge (mySort xs1) (mySort xs2)
            where 
              (xs1, xs2) = splitAt (quot (length xs) 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xrest) (y:yrest) | x <= y = x: merge xrest (y:yrest)
merge (x:xrest) (y:yrest) | otherwise = y: merge (x:xrest) yrest



emptyBoard :: Board
emptyBoard = [Left 1, Left 2, Left 3, Left 4, Left 5, Left 6, Left 7, Left 8, Left 9]

showSq :: Square -> String
showSq (Left a) = show a
showSq (Right b) = show b

showBoard :: Board -> String
showBoard (a:b:c:d:e:f:g:h:i:_) =  showSq a ++ sep ++ showSq b ++ sep ++ showSq c ++ rowSep ++
                                   showSq d ++ sep ++ showSq e ++ sep ++ showSq f ++ rowSep ++
                                   showSq g ++ sep ++ showSq h ++ sep ++ showSq i
                                   where rowSep = "\n---------\n"
                                         sep = " | "
showBoard _ = "Invalid Board dimensions, 3x3 required"

{- Shifts the turn to the next player -}
next :: Player -> Player
next X = O
next O = X

wins :: Player -> Board -> Bool
wins m (a:b:c:d:e:f:g:h:i:_) = (same m a b c) || -- top row win
                               (same m d e f) || -- middle row win
                               (same m g h i) || -- bottom row win
                               (same m a d g) || -- left col win
                               (same m b e h) || -- middle col win
                               (same m c f i) || -- right col win
                               (same m a e i) || -- diagonal from top left win
                               (same m g e c)    -- diagonal from bottom left win
wins _ _ = False

same :: Player -> Square -> Square -> Square -> Bool
same m (Right m1) (Right m2) (Right m3) = m == m1 && m == m2 && m == m3
same _ _ _ _ = False

boardNextMoves :: Player -> Board -> [Board]
boardNextMoves m b = foldr (\ n r -> add m (b !! (n-1)) b r) [] [1..9]

add :: Player -> Square -> Board -> [Board] -> [Board]
add m (Left n) b r = (setSq n m b):r
add _ (Right _) _ r = r

setSq :: Int -> Player -> Board -> Board
setSq 1 m (_:bs) = (Right m):bs
setSq n m (b:bs) = b:(setSq (n-1) m bs)
setSq _ _ [] = []

{-
Player one is the player's 
1. Populate the tree with every possible Player at a level and assign a +/- value indicating if you win or lose on it
2. Recurse on this pattern, alternating players' moves
3. 
-} 

\end{code}
\noindent \texttt
\end{code}
\end{document}