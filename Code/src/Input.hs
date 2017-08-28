module Input(handleInput) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Data.Binary
import Data.ByteString.Lazy as B
import Board
import AI

import Debug.Trace

-- Update the world state given an input event.
handleInput :: Event -> World -> IO World
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w
    = if (options w) || (game_over w) then case (x,y) of
            _ -> do return w
      else if (turn w == h_player w) then
            do case getCoord x y w of
                Nothing -> return (trace ("Invalid Move ") w)
                Just b  -> return (trace ("Piece Played") w{ previous_world = w, board = b, turn = other (turn w) })
      else do return (trace("AI turn") w)

handleInput (EventKey (Char k) Down _ _) w =
    if (options w) then
      do case k of
          'h' -> return w{ ai_level = 3, flags = (flags w){ hardAi = True, easyAi = False }} -- Hard Ai
          'm' -> return w{ ai_level = 2, flags = (flags w){ hardAi = False, easyAi = False }} -- Medium Ai
          'e' -> return w{ ai_level = 1 ,flags = (flags w){ hardAi = False, easyAi = True }} -- Easy Ai
          'l' -> return w{ flags = (flags w){ largeB = True, smallB = False }} -- 10x10 board
          'a' -> return w{ flags = (flags w){ largeB = False, smallB = False }} -- 8x8 Board
          's' -> return w{ flags = (flags w){ largeB = False, smallB = True }} -- 6x6 Board
          'r' -> return w{ flags = (flags w){ random = True }}-- Random Start
          'n' -> return w{ flags = (flags w){ random = False }}-- Normal Start
          'q' -> return w{ options = False } -- Exit
          (_) -> return w
    else
      do case k of
          'n' -> (trace ("New game") makeWorld $ flags w)
          'h' -> return (trace ("Hints") w{ hints = not $ hints w })
          'u' -> return (trace ("Undo move") previous_world $ previous_world w)
          'o' -> return (trace ("Options menu") w{ options = True })
          's' -> do (trace("Saving game") B.writeFile "SaveFile" (encode w))
                    return w
          'l' -> do file <- B.readFile "SaveFile"
                    return $ (trace("Loading game") decode file)
          (_) -> return (trace ("Invalid key") w)

handleInput e w  = do return w

-- Returns the coordinates of the board pressed from the coordinates pressed on the screen
getCoord :: Float -> Float -> World -> Maybe Board
getCoord x y w = makeMove (board w) (turn w) (x',y')
    where
        	x' = floor ((x+200) / 72.0) + (size (board w) `div` 2)
        	y' = floor (y/72.0) + (size (board w) `div` 2)
