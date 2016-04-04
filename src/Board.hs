module Board where

data Col = Black | White
  deriving Show

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
initBoard = Board 8 0 [((3,3), Black), ((3, 4), White),
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
makeMove = undefined
-- makeMove b col pos = if insideBoard b pos && positionFree b pos && flipCheck b col pos then # Update Board

-- Returns true if inside board else false
insideBoard :: Board -> Position -> Bool
insideBoard board (x,y) = x >= 0 && y>=0 && x < size board && y < size board

-- Returns true if position free else false
positionFree :: Board -> Position -> Bool
positionFree board pos = foldl (\acc p -> if (fst p) == pos then False else acc) True (pieces board)

flipCheck :: Board -> Col -> Position -> (Board, Bool)
flipCheck = undefined

horizontalCheck :: Board -> Col -> Position -> (Board, Bool)
horizontalCheck = undefined

leftCheck :: Board -> Col -> Position -> Bool -> ()
leftCheck b col (x,y) seen = case seen of
                               True ->
                               False -> case find   of
                                          Nothing -> False
                                          _ -> snd

-- Returns colour at positon given
positionColour :: Board -> Position -> Maybe [(Position,Col)]
positionColour board pos = find (\z -> fst z = pos) (pieces board)

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
evaluate = undefined
