G52AFP Coursework 1 - Connect 4
   
Hangjian Yuan
psyhy4@nottingham.ac.uk

----------------------------------------------------------------------

> import Data.Char
> import Data.List
> import System.IO

----------------------------------------------------------------------

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows :: Int
> rows = 6
>
> cols :: Int
> cols = 7
>
> win :: Int
> win = 4
>
> depth :: Int
> depth = 6

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:

> type Board = [Row]
>
> type Row = [Player]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = replicate cols '-'
>                  nums    = take cols ['0'..]
>
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

----------------------------------------------------------------------
Section 1 - Built-in Boards

This test board is given in intial code, it is used to test functions'
correctness.

> test :: Board 
> test = [[B,B,B,B,B,B,B], 
>         [B,B,B,B,B,B,B], 
>         [B,B,B,B,B,B,B], 
>         [B,B,B,X,X,B,B], 
>         [B,B,O,O,X,B,B], 
>         [B,O,O,X,X,X,O]]


This empty board is used as the start of a game.

> empty :: Board
> empty = replicate rows (replicate cols B)

----------------------------------------------------------------------
Section 2 - Player Functions

This function change the player in an alternate way.

> next :: Player -> Player
> next O = X
> next X = O

(The reason I haven't define turn function is that my minimax function
takes a Player parameter, see the following definiton.)

----------------------------------------------------------------------
Section 3 - Extract rows, cols, diagonals

This section contains three functions used to extract rows, cols and
diagonals respectively. (The idea comes from sudoku example)

Extracting rows:

> getRow :: Board -> [Row]
> getRow = id


Extracting cols:

> getCol :: Board -> [Row]
> getCol = transpose


Extracting diagonals:

> type Pos = (Int, Int)

This function generate a list of positions, each one is the start position
of one diagonal. (This set is just the union of first row and first col)

> startPos :: Int -> Int -> [Pos]
> startPos rs cs = [(0, y) | y <- [0..cs-1]] ++ [(x, 0) | x <- [1..rs-1]]


This function accept a start position of one diagonal, and output a list of 
positions, which compose this diagonal.

> diagPos :: Pos -> [Pos]
> diagPos (x, y) = zip [x..rows-1] [y..cols-1]


This function output a diagonal using the given position list.

> pos2diag :: Board -> [Pos] -> Row
> pos2diag b ps = [ b !! x !! y | (x,y) <- ps]


This function output all the diagonals whose direction is the same as the 
maindiagonal.

> mainDiag :: Board -> [Row]
> mainDiag b = [pos2diag b (diagPos p) | p <- (startPos rows cols)]


This function output all the diagonals. (cover both directions)

> getDia :: Board -> [Row]
> getDia b = mainDiag b ++ mainDiag (map reverse b)

----------------------------------------------------------------------
Section 4 - Judging Functions

This section contains functions judging whether one player is win or the
board is full.


This function accept a player and a row, checking whether exist a winning
case in this row.

> hasRow :: Player -> Row -> Bool
> hasRow _ r | length r < win = False
> hasRow p r = all (p==) (take win r) || hasRow p (tail r)


This function accept a player and a board, checking whether exist a winning
case in the board.

> hasWon :: Player -> Board -> Bool
> hasWon p b = any (hasRow p) (getRow b ++ getCol b ++ getDia b)


This function check whether the board is full.

> full :: Board -> Bool
> full = all (/= B) . concat


This function check whether the game is finish. (One player is win, or the
board is full.)

> finish :: Board -> Bool
> finish b = hasWon O b || hasWon X b || full b

----------------------------------------------------------------------
Section 5 - Move Function

This section contains move function and its helper functions.


This function accept a player, the chosen column, and the board.
Then it output the new board by recording the input move.

> move :: Player -> Int -> Board -> Board
> move p n b = getCol (take n col ++ [update p (col !! n)] ++ drop (n + 1) col)
>              where
>                 col = getCol b


This function accept a player and a column, and output the new column
which record the new move.
(The function type might be confusing, it accept a column, not a row)

> update :: Player -> Row -> Row
> update p r = init (takeWhile (== B) r) ++ [p] ++ dropWhile (== B) r


This function validate the chosen column number.
(It is used in run' function, not in move function, I put it in this
section just for clarity)

> valid :: Board -> Int -> Bool
> valid b n = 0 <= n && n < cols && (head b) !! n == B

----------------------------------------------------------------------
Section 6 - IO Helper

This section contains two IO helper functions: getNat, prompt.


This function get user's input, check whether it's a valid number,
if so, return the corresponding integer; otherwise, prompt an error
message.

> getNat :: String -> IO Int
> getNat prompt = do putStr prompt
>                    xs <- getLine
>                    if xs /= [] && all isDigit xs then
>                       return (read xs)
>                    else
>                       do putStrLn "ERROR: Invalid number"
>                          getNat prompt


This function is used for generating a prompt sentence.

> prompt :: Player -> String
> prompt p = "Player " ++ show p ++ ", enter your move: "

----------------------------------------------------------------------
Section 7 - Main

This section contain the main function and its helper functions.


This is the main function of the program, the first action in do sequence
turnoff the output buffer, otherwise, the board won't show up when running
the program.

> main :: IO ()
> main = do hSetBuffering stdout NoBuffering 
>           run empty O


This function just show the board and call another helper function: run'.

> run :: Board -> Player -> IO ()
> run b p = do showBoard b
>              run' b p


This function accept a board and a player.
First, it applies judging functions to the board checking whether the game is
over. If so, it output proper message.
Second, it checks the player type, if the player is human(O), it get an integer
from input, and update the board. If the player is computer, it update the 
board using the minimax tree.

> run' :: Board -> Player -> IO ()
> run' b p | hasWon O b = putStrLn "Player O wins!\n"
>          | hasWon X b = putStrLn "Player X wins!\n"
>          | full b     = putStrLn "It's a draw!\n"
>          | p == O     = do i <- getNat (prompt p)
>                            case valid b i of
>                               False -> do putStrLn "ERROR: Invalid move"
>                                           run' b p
>                               True  -> run (move p i b) (next p)
>          | p == X     = do putStrLn "Player X is thinking..."
>                            (run $! (bestmove b p)) (next p)

----------------------------------------------------------------------
Section 8 - Game Tree

This section contains the game tree definiton and related functions.

> data Tree a = Node a [Tree a]
>               deriving Show


This function generates the whole game tree using the given board and player
as the start.
(The whole tree is really huge, however haskell uses lazy evaluation)

> gametree :: Board -> Player -> Tree Board
> gametree b p = Node b [gametree b' (next p) | b' <- moves b p]


This helper function accept a board and a player, then it computes all the
possible valid next step, and outputs all the corresponding new boards.

> moves :: Board -> Player -> [Board]
> moves b p | finish b = []
>           | otherwise = [move p n b | n <- filter (valid b) [0..cols-1]]


This function is used to control the depth of the game tree.

> prune :: Int -> Tree a -> Tree a
> prune 0 (Node x _)  = Node x []
> prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

----------------------------------------------------------------------
Section 9 - Minimax

This section implements minimax algorithm based on the game tree section.


This function labels the leaves in the game tree.

> llabel :: Board -> (Player, Board)
> llabel b | hasWon O b = (O, b)
>          | hasWon X b = (X, b)
>          | otherwise  = (B, b)


This function labels the nodes in the game tree.

> nlabel :: Player -> [Tree (Player, Board)] -> Player
> nlabel p ts | p == O = minimum [p' | Node (p', b) _ <- ts]
>             | p == X = maximum [p' | Node (p', b) _ <- ts]


This function accepts a player and a tree of board, and outputs a tree of
(Player, Board), each board has a player parameter, indicating which player
has high possibility to win.

> minimax :: Player -> Tree Board -> Tree (Player, Board)
> minimax p (Node x []) = Node (llabel x) []
> minimax p (Node x ts) = Node ((nlabel p subtrees), x) subtrees
>                         where
>                            subtrees = [minimax (next p) t | t <- ts]


This function creates a minimax tree of the given board and player,
then it selects the first child node whose label is the same as the root,
and outputs the corresponding board.

> bestmove :: Board -> Player -> Board
> bestmove b p = head [board | Node (player, board) _ <- ts, player == p']
>                where
>                   Node (p', _) ts = minimax p (prune depth (gametree b p))

----------------------------------------------------------------------
