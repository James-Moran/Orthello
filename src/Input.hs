module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace

-- Update the world state given an input event.
handleInput :: Event -> World -> World
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w
    = if (turn w == Black) then case function x y w of
        Nothing -> trace ("Invalid Move") w -- if nothing, return the old world
        Just b  -> trace ("Piece Played at:" ++ show (x,y)) w{ previous_world = w, board = b, turn = other (turn w) } -- if the function returned a board, show the x, y and update the board also update the turn.
                   else trace("AI turn") w
handleInput (EventKey (Char k) Down _ _) w = case k of
        'n' -> trace ("New game") initWorld
        'h' -> trace ("Hints") w{ hints = not $ hints w }
        'u' -> trace ("Undo move") previous_world $ previous_world w
        _   -> trace ("Invalid key") w
handleInput e w  = w

function :: Float -> Float ->  World -> Maybe Board
function x y w = makeMove (board w) (turn w) (x',y')
    where
-- floor(x(y)/80) will return an integer from -4 to 3, add half of the board size to get the position in board.
-- for example, x:-20 y:-20 will return          x':-1+4=3      y':4-0 =4 which is the bottom left white piece at (3,4)
	x' = floor (x / 80.0) + (size (board w) `div` 2)
	y' = (size (board w) `div` 2)- ceiling (y/80.0)



{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.
 At first, it is reasonable to assume that both players are human players.
-}
