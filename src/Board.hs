module Board where

import Data.List

data Col = Black | White
  deriving (Show, Eq)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)



-- A Board is a record containing the board size (a board is a square grid, n *
-- n), the number of consecutive passes, and a list of pairs of position and
-- the colour at that position.

data Board = Board { size :: Int,
                     passes :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving Show

-- Default board is 8x8, neither played has passed, with 4 initial pieces
initBoard = Board 8 0 [((3,3), Black), ((3,4), White),
                       ((4,3), White), ((4,4), Black)]

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col }

initWorld = World initBoard Black

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, there is a piece already there,
-- or the move does not flip any opposing pieces)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board col pos = if valid board pos then flipMove board col pos else Nothing

-- Tests if the position is within the board and not taken
valid :: Board -> Position -> Bool
valid b p = let insideBoard board (x,y) = x >= 0 && y>=0 && x < size board && y < size board
                positionFree board pos = foldl (\acc p -> if (fst p) == pos then False else acc) True (pieces board)
            in insideBoard b p && positionFree b p

-- Flips the opposing peices captured and adds the piece placed
flipMove :: Board -> Col -> Position -> Maybe Board
flipMove b c p = let flipCaptured = (b,False)         ?>
                      dirMove c p ((+1),(+0))         ?>
                      dirMove c p ((+0),(+1))         ?>
                      dirMove c p ((subtract 1),(+0)) ?>
                      dirMove c p ((+0),(subtract 1)) ?>
                      dirMove c p ((+1),(+1))         ?>
                      dirMove c p ((+1),(subtract 1)) ?>
                      dirMove c p ((subtract 1),(+1)) ?>
                      dirMove c p ((subtract 1),(subtract 1))
                 in if (snd flipCaptured) then Just (userMove (fst flipCaptured) c p) else Nothing

-- Adds the move made to the board
userMove :: Board -> Col -> Position -> Board
userMove b c p = Board (size b) (passes b) ((p,c):(pieces b))

-- Keeps new board if flipping of peices is successful, bool maintains if any flip directions has been successful
(?>) :: (Board, Bool) -> (Board -> Maybe Board) -> (Board, Bool)
(?>) (currBoard, result) func = case (func currBoard) of
                                  Just newBoard -> (newBoard, True)
                                  Nothing       -> (currBoard, result)

-- Checks next piece in the specified direction is of the opposite colour, if so flip it, else return nothing
dirMove :: Col -> Position -> ((Int->Int),(Int->Int)) ->  Board -> Maybe Board
dirMove col (x,y) (op1,op2) board = case positionColour board (op1 x, op2 y) of
                                      Just (p, c) -> if (other col) == c then dirMove' col (op1 x, op2 y) (op1,op2) (changeCol board p) else Nothing
                                      Nothing     -> Nothing

-- Flips the opposing pieces until it reaches one of the original colour, if it doesn't find one it returns nothing
dirMove' :: Col -> Position -> ((Int->Int),(Int->Int)) -> Board -> Maybe Board
dirMove' col (x,y) (op1,op2) board = case positionColour board (op1 x, op2 y) of
                                      Just (p, c) -> if col == c then (Just board ) else dirMove' col (op1 x, op2 y) (op1,op2) (changeCol board p)
                                      Nothing     -> Nothing

-- Changes the colour of the position given
changeCol :: Board -> Position -> Board
changeCol board pos = Board (size board) (passes board) (foldl (\acc (p, c) -> if (p == pos) then (p, other c) : acc else (p, c) : acc) [] (pieces board))

-- Returns colour at positon given
positionColour :: Board -> Position -> Maybe (Position,Col)
positionColour board pos = find (\z -> fst z == pos) (pieces board)

-- Check the current score
-- Returns a pair of the number of black pieces, and the number of
-- white pieces
checkScore :: Board -> (Int, Int)
checkScore board = foldl (\(x,y) p -> case (snd p) of
                                          Black -> (x+1, y)
                                          White -> (x, y+1)) (0,0) (pieces board)

-- Return true if the game is complete (that is, either the board is
-- full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver board = if passes board == 2 || length (pieces board) == size board * size board then True else False

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate board col = foldl (\acc p -> if snd p == col then acc+1 else acc-1) 0 (pieces board)
