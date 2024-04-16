module TicTacToe (
    Grid,
    Player(..),
    size,
    emptyGrid,
    next,
    isWinner,
    fullBoard,
    move
) where

import Data.Char
import Data.List

-- Size of the board
size :: Int
size = 3

type Grid = [[Player]]

exampleGrid :: Grid
exampleGrid = [[X, B, O], [O, X, B], [B, O, X]]

data Player = O | B | X
    deriving (Eq, Show, Ord)

-- Function to swap between X and O
next :: Player -> Player
next O = X
next X = O
next B = B

-- >>> 1 + 2

-- Function to create an empty grid
emptyGrid :: Grid
emptyGrid = replicate size (replicate size B)

-- Note, replicate takes an int n and an a
-- and produces a list of length n as. For example:

-- >>> replicate 5 'a'
-- "aaaaa"

-- >>> replicate 5 1
-- [1,1,1,1,1]

-- Predicate function to check if there are no more blank
-- spaces left on the board
fullBoard :: Grid -> Bool
fullBoard = all (notElem B)

-- Remember that the all function takes a predicate
-- and checks if every element of the lists passes the predicate.
-- notElem is just the dual of elem it checks if the given
-- element is not an element of the given list.
-- Notice the type signature of notElem B:
-- >>> :t notElem B
-- notElem B :: Foldable t => t Player -> Bool

-- Then the type of all (notElem B)
-- >>> :t all (notElem B)
-- all (notElem B) :: (Foldable t1, Foldable t2) => t1 (t2 Player) -> Bool

-- Function to determine whose turn it is given a grid.
-- Here we use the trick of flattening the list to check
-- a predicate for every element of the list of lists without
-- having to worry about nested lists.
whoseTurn :: Grid -> Player
whoseTurn g = if os <= xs then O else X
    where
        ps = concat g
        os = length (filter (== O) ps)
        xs = length (filter (== X) ps)

-- Predicate function to decide if the given
-- player is the winner of the given grid
isWinner :: Player -> Grid -> Bool
isWinner p g = any line (rows ++ cols ++ dias)
    where
        line = all (== p)
        rows = g
        cols = transpose g
        dias = [ diag g, diag (map reverse g) ]

-- transpose takes a list and turns the rows into
-- columns and vice versa. For example:
-- >>> transpose [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]

-- >>> transpose exampleGrid
-- [[X,O,B],[B,X,O],[O,B,X]]

-- Function to return the diagonal from top left
-- to bottom right
diag :: Grid -> [Player]
diag g = [ g !! n !! n | n <- [0..(size - 1)] ]

-- >>> diag exampleGrid
-- [X,X,X]

-- Now we can check if any has won yet
hasWinner :: Grid -> Bool
hasWinner g = isWinner O g || isWinner X g

-- Function to check if a given move is in bounds
validMove :: Grid -> Int -> Bool
validMove g i =
    0 <= i && i < size^2 && concat g !! i == B

-- Note we are using the convention of counting from
-- the top left to the bottom right row by row starting
-- at 0. For example:
-- 0 | 1 | 2
-- 3 | 4 | 5
-- 6 | 7 | 8

-- >>> validMove exampleGrid 1
-- True

-- Out of bounds on the left
-- >>> validMove exampleGrid (-1)
-- False

-- Out of bounds on the right
-- >>> validMove exampleGrid 9
-- False

-- Not blank
-- >>> validMove exampleGrid 0
-- False

-- Function which attempts to make a move for a player
-- at the given index. Returns Nothing if it fails
-- and Just with a new grid if it succeeds
move :: Grid -> Int -> Player -> Maybe Grid
move g i p
    | not (validMove g i) = Nothing
    | otherwise           =
        Just (chop size (xs ++ [p] ++ ys))
    where
        (xs, B:ys) = splitAt i (concat g)

-- >>> move exampleGrid 1 X
-- Just [[X,X,O],[O,X,B],[B,O,X]]

-- >>> move exampleGrid 0 X
-- Nothing

-- >>> move exampleGrid 6 O
-- Just [[X,B,O],[O,X,B],[O,O,X]]

-- Technically it lets B move so be careful
-- >>> move exampleGrid 6 B
-- Just [[X,B,O],[O,X,B],[B,O,X]]

-- Our old friend chop
chop :: Int -> [a] -> [[a]]
chop n = unfoldr go
    where
        go [] = Nothing
        go xs = Just (splitAt n xs)

-- >>> chop 8 [1..10]
-- [[1,2,3,4,5,6,7,8],[9,10]]


