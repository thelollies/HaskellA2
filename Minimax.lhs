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
module Minimax(wins, emptyBoard, showBoard, Move(X,O)) where

data Move = X | O
type Square = Either Int Move
type Board = [Square]

instance Show Move where
  show X = "X"
  show O = "O"

instance Eq Move where
  X == X = True
  O == O = True
  _ == _ = False

{-instance Show Square where
  show (Left a) = show a
  show (Right b) = show b-}

main :: IO ()
main = putStrLn (showBoard emptyBoard) 

emptyBoard :: Board
emptyBoard = [Left 1, Left 2, Left 3, Left 4, Left 5, Left 6, Left 7, Left 8, Left 9]

showSq :: Square -> String
showSq (Left a) = show a
showSq (Right b) = show b

showBoard :: Board -> String
showBoard (a:b:c:d:e:f:g:h:i:_) =  show a ++ sep ++ show b ++ sep ++ show c ++ rowSep ++
                                   show d ++ sep ++ show e ++ sep ++ show f ++ rowSep ++
                                   show g ++ sep ++ show h ++ sep ++ show i
                                   where rowSep = "\n---------\n"
                                         sep = " | "
showBoard _ = "Invalid Board dimensions, 3x3 required"

{- Shifts the turn to the next player -}
next :: Move -> Move
next X = O
next O = X

wins :: Move -> Board -> Bool
wins m (a:b:c:d:e:f:g:h:i:_) = (same m a b c) || -- top row win
                               (same m d e f) || -- middle row win
                               (same m g h i) || -- bottom row win
                               (same m a d g) || -- left col win
                               (same m b e h) || -- middle col win
                               (same m c f i) || -- right col win
                               (same m a e i) || -- diagonal from top left win
                               (same m g e c)    -- diagonal from bottom left win
wins _ _ = False

same :: Move -> Square -> Square -> Square -> Bool
same m (Right m1) (Right m2) (Right m3) = m == m1 && m == m2 && m == m3
same _ _ _ _ = False

boardSubsets :: Move -> Board -> Board
boardSubsets m b = foldr (\ n r -> add m (b !! n) b r) [] [0..9]

add :: Move -> Square -> Board -> [Board] -> [Board]
add m (Left n) r b = (setSq n m b):r
add m (Right _) r b = r

setSq :: Int -> Move -> Board -> Board
setSq 0 m (b:bs) = (Right m):bs
setSq n m (b:bs) = b:(setSq (n-1) m bs)

{-
Move one is the player's 
1. Populate the tree with every possible move at a level and assign a +/- value indicating if you win or lose on it
2. Recurse on this pattern, alternating players' moves
3. 
-} 

\end{code}
\noindent \texttt
\end{code}
\end{document}