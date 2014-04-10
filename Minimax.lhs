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
type Board = [[Square]]

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
emptyBoard = [[Left 1, Left 2, Left 3],[Left 4, Left 5, Left 6],[Left 7, Left 8, Left 9]]

showSq :: Square -> String
showSq (Left a) = show a
showSq (Right b) = show b

showBoard :: Board -> String
showBoard ((a:ar):(b:br):(c:cr):_) =  
                        (showSq a) ++ foldr (\ e r -> " | " ++ (showSq e) ++ r) rowSep ar  ++ 
                        (showSq b) ++ foldr (\ e r  -> " | " ++ (showSq e) ++ r) rowSep br  ++ 
                        (showSq c) ++ foldr (\ e r -> " | " ++ (showSq e) ++ r) "" cr
                          where rowSep = "\n---------\n"
showBoard _ = "Invalid Board dimensions, 3x3 required"

{- Shifts the turn to the next player -}
next :: Move -> Move
next X = O
next O = X

wins :: Move -> Board -> Bool
wins m ((a:b:c:_):(d:e:f:_):(g:h:i:_):_) = (same m a b c) || -- top row win
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