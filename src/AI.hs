module AI where

import Board
import Data.Maybe

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from
                             -- here for opposite player

moveGenerator :: Board -> Col -> [Position]
moveGenerator board c = [(x,y) | x <- [0..(size board)], y <- [0..(size board)]]

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove depth tree = fst $ getMax $ foldl (\acc moves -> (minPos (depth-1) (fst moves) (snd moves)):acc) [] (next_moves tree)

-- Returns the maximum scoring position to play and associated score it would have at max depth
-- Of either the next moves evaluated (if depth == 0) or the rest of the tree evaluated by minPos
maxPos :: Int -> Position -> GameTree -> (Position, Int)
maxPos depth pos tree = if null (next_moves tree) then (pos, evaluate (game_board tree) (game_turn tree))
                        else if (depth == 0) then getMax (maxPosEvalList pos tree)
                        else getMax $ foldl (\acc moves -> (minPos (depth-1) pos (snd moves)):acc) [] (next_moves tree)

-- Returns the minimum scoring move to play next
minPos :: Int -> Position -> GameTree -> (Position, Int)
minPos depth pos tree = if null (next_moves tree) then (pos, evaluate (game_board tree) (other $ game_turn tree))
                        else if (depth == 0) then getMin (minPosEvalList pos tree)
                        else getMin $ foldl (\acc moves -> (maxPos (depth-1) pos (snd moves)):acc) [] (next_moves tree)

-- Returns the largest scoring position to play from a list of moves
getMax :: [(Position, Int)] -> (Position, Int)
getMax (x:xs) = foldl (\x acc -> if snd x > snd acc then x else acc) x xs

-- Returns the smallest scoring position to play from a list of moves
getMin :: [(Position, Int)] -> (Position, Int)
getMin (x:xs) = foldl (\acc x -> if snd x < snd acc then x else acc) x xs

minPosEvalList :: Position -> GameTree -> [(Position, Int)]
minPosEvalList pos tree = foldl (\acc moves -> (pos, evaluate (game_board tree) (other $ game_turn tree)):acc) [] (next_moves tree)

maxPosEvalList :: Position -> GameTree -> [(Position, Int)]
maxPosEvalList pos tree = foldl (\acc moves -> (pos, evaluate (game_board tree) (game_turn tree)):acc) [] (next_moves tree)

-- Update the world state after some time has passed
updateWorld :: Float -> World -> World -- Float is time since last update
updateWorld t w = let tree = (buildTree (moveGenerator) (board w) (turn w))
                  in if gameOver $ board w then w{ game_over = True }
                     else if null $ next_moves tree then w{ previous_world = w, board = (board w){ passes = (passes $ board w) + 1}, turn = other (turn w) }
                     else if (turn w == (other $ h_player w)) then w{ previous_world = w, board = fromJust(makeMove (board w) (turn w) (getBestMove 1 tree)), turn = other (turn w) }
                     else w

{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random valid move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either
 player has won and display a message if so.
-}
