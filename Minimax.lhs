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

data SQ = X | O | E
type Player = SQ
type Board = [SQ]
data MTree = Node Board Player [MTree] | Win Board Player

instance Eq MTree where
  (Win _ _) == (Win _ _) = True
  (Win _ _) == (Node _ _ _) = False
  (Node _ _ _) == (Win _ _) = False
  l@(Node _ _ _) == r@(Node _ _ _) = minimax 3 l == minimax 3 r


instance Ord MTree where
  (Win _ _) `compare` (Win _ _) = EQ
  (Win _ _) `compare` (Node _ _ _)        = GT
  (Node _ _ _) `compare` (Win _ _)        = LT
  l@(Node _ _ _) `compare` r@(Node _ _ _) = minimax 3 l `compare` minimax 3 r

instance Show MTree where
  show (Node b _ _) = showBoard b
  show (Win b _) = showBoard b

instance Show SQ where
  show X = "X"
  show O = "O"
  show E = " "

instance Eq SQ where
  X == X = True
  O == O = True
  E == E = True
  _ == _ = False

main :: IO()
main = do
          putStrLn "Welcome to Tic Tac Toe, you get first turn.\n"
          putStrLn "Make your selection by entering 1-9 where those numbers "
          putStrLn "match an empty slot on the board. The board is numbered "
          putStrLn "left to right, top to bottom.\n\n"
          play (gameSubTree X emptyBoard)

play :: MTree -> IO ()
play (Node brd _ []) = putStrLn (showBoard brd ++ "\n\n" ++ "It's a draw!")
play (Node brd X subtree) = putStrLn (showBoard brd) >> chooseMove brd subtree >>= play
play (Node _ O subtree) = play (last (mySort subtree))
play (Win brd a) = putStrLn (showBoard brd ++ "\n\nPlayer " ++ show a ++ " wins!\n")
play (Node _ E _) = error "Cannot play tic tac toe as an empty tile"

chooseMove :: Board -> [MTree] -> IO MTree
chooseMove brd options = do 
                          n <- getLine
                          if (fst (setSq (read n - 1 :: Int) X brd) == E) then
                            return $ findMove (snd (setSq (read n - 1 :: Int) X brd)) options
                          else
                            putStrLn (showBoard brd) >> chooseMove brd options                        

findMove :: Board -> [MTree] -> MTree
findMove brd tree = head $ foldr (\ n@(Node b _ _) r -> select (eqBrd b brd) n r) [] tree
                where select True e r = e:r
                      select False _ r = r

eqBrd :: Board -> Board -> Bool
eqBrd (b:rd) (b2:rd2) = b == b2 && eqBrd rd rd2
eqBrd [] [] = True
eqBrd _ _ = False


comp :: Player
comp = O

emptyBoard :: Board
emptyBoard = [E,E,E,E,E,E,E,E,E]

{- Decomposes a board into it's winnable rows/columns/diagonals -}
threes :: Board -> [(SQ, SQ, SQ)]
threes (a:b:c:d:e:f:g:h:i:_) = [(a, b, c), (d, e, f), (g, h, i), -- rows
                                (a, d, g), (b, e, h), (c, f, i), -- columns
                                (a, e, i), (g, e, c)]            -- diagonals
threes _ = error "Board must be 3x3"

showBoard :: Board -> String
showBoard (a:b:c:d:e:f:g:h:i:_) =  show a ++ sep ++ show b ++ sep ++ show c ++ rowSep ++
                                   show d ++ sep ++ show e ++ sep ++ show f ++ rowSep ++
                                   show g ++ sep ++ show h ++ sep ++ show i ++ "\n"
                                   where rowSep = "\n---------\n"
                                         sep = " | "
showBoard _ = "Invalid Board dimensions, 3x3 required"

gameSubTree :: Player -> Board -> MTree
gameSubTree player board | gameOver board = Win board (next player) -- The game was won by the player who just went
gameSubTree player board = Node board player $ foldr (\ e r -> (gameSubTree (next player) e) : r) [] (boardNextMoves player board)

{- Returns the score for the board in the given tree node by searching to
   the specified depth and applying the evaluation function on the successive
   moves -}
minimax :: Int -> MTree -> Int
minimax n (Node board _ children) | n == 0 || length children == 0 = evaluate  board
minimax _ (Win board _) = evaluate board
minimax n (Node _ p children) = foldr ((maxOrMin p) . (minimax (n-1))) (startVal p) children -- possible sub moves
                                   where maxOrMin p1 | p1 == comp = max 
                                                     | otherwise = min
                                         startVal p1 | p1 == comp = (-999999999 :: Int)
                                                     | otherwise = (999999999 :: Int)

{- Determines whether a player has won the game -}
gameOver :: Board -> Bool
gameOver board = foldr (\ (a, b, c) r -> r || (a == b && a == c && a /= E)) False $ threes board

{- Calculates the value of the board from the computer's perspective -}
evaluate :: Board -> Int
evaluate board = foldr ((+) . evaluateLine) 0 (threes board)

{- Takes three squares and calculates the scoreof the specified line 
   relative to the computer. It awards 100, 10, 1 for 3, 2, 1 in a line
   respectively and negates the value for the human player -}
evaluateLine :: (SQ, SQ, SQ) -> Int
evaluateLine (s1, s2, s3) = tileThr s3 $ tileTwo s2 $ tileOne s1
                          where tileOne s    | s == comp = 1
                                             | s == next comp = -1
                                             | otherwise = 0
                                tileTwo s n  | s == comp && n == 1 = 10
                                             | s == next comp && n == 1 = 0
                                tileTwo s n  | s == comp && n == -1 = 0
                                             | s == next comp && n == -1 = -10
                                tileTwo s n  | s == comp && n == 0 = 1
                                             | s == next comp && n == 0 = -1
                                tileTwo _ n = n
                                tileThr s n  | s == comp && n > 0 = n * 10
                                             | s == comp && n < 0 = 0
                                             | s == comp && n == 0 = 1
                                             | s == next comp && n < 0 = n * 10 
                                             | s == next comp && n > 1 = 0
                                             | s == next comp = -1
                                             | otherwise = n

{- Generates the possible moves by the specified player from the specified square.
   Returns the moves as boards with the move placed on them. The Int pertaining
   to the square to start at is used by the recursion rather than the  -}
boardNextMoves :: Player -> Board -> [Board]
boardNextMoves p brd = foldr (\(n, b) r -> add n p b r) [] $ [0..8] `zip` (replicate 9 brd)

add :: Int -> Player -> Board -> [Board] -> [Board]
add n p brd brds | sq == E = ((newBoard):brds)
                 where (sq, newBoard) = setSq n p brd
add _ _ _ brds = brds

{- Sets the specified square and returns a tuple (old sq, new board) -}
setSq :: Int -> Player -> Board -> (SQ, Board)
setSq 0 p (b:bs) = (b, p:bs)
setSq n p (b:bs) = (square, b:board)
                 where (square, board) = (setSq (n-1) p bs)
setSq _ _ [] = (E, [])

next :: Player -> Player
next X = O
next O = X
next E = E


\end{code}

\noindent \texttt{mySort} accepts a list of elements which are a subtype of \texttt{Ord} and returns
that list in ascending order. It uses recursive mergesort to achieve the sorting.

\begin{code}
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

\end{code}
\noindent \texttt
\end{code}
\end{document}