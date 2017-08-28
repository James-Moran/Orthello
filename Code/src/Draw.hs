module Draw(drawWorld) where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Monoid
import Board
import AI

-- Calls the correct draw function depending on the state of the world
drawWorld :: [Picture] -> World -> IO Picture
drawWorld p w = if (game_over w) then drawGameOver p w else if (options w) then drawOptions p w else drawBoard p w

-- Draws the game over screen
drawGameOver :: [Picture] -> World -> IO Picture
drawGameOver p w = do return (pictures [(p !! 0) , whiteResult, blackResult])
                             where
                               whiteResult = translate (0) (-200) $ scale (0.4) (0.4) $ text $ "White: " ++ (show $ snd $ checkScore (board w))
                               blackResult = translate (-300) (-200) $ scale (0.4) (0.4) $ text $ "Black: " ++ (show $ fst $ checkScore (board w))


-- Draws the background, the board and then the pieces, also possibly the hints
drawBoard :: [Picture] -> World -> IO Picture
drawBoard p w = do return result
              where
               result = pictures [background, mconcat boardPieces, whiteResult, blackResult, colour, hintsPicture]
               background = case (size $ board w) of
                              10 -> (p !! 1)
                              8  -> (p !! 2)
                              6  -> (p !! 3)
               sizeOfCell = 80
               boardPieces = map (drawPiece p (size $ board w)) (pieces (board w))
               colour = Translate (180) (100) $ scale (0.4) (0.4) $ color blue $ text $ show (turn w)
               blackResult = Translate (180) (-100) $ scale (0.4) (0.4) $ color blue $ text (show $ fst $ checkScore (board w))
               whiteResult = Translate (180) (-300) $ scale (0.4) (0.4) $ color blue $ text (show $ snd $ checkScore (board w))

               hintsPicture = if (hints w)
                 then drawHints (p !! 7) (size $ board w) w else blank

-- Draws the pieces in their correct position
drawPiece :: [Picture] -> Int -> (Position, Col) -> Picture
drawPiece p size' ((x,y), col) = piece
         where
          size = fromIntegral(size')/2
          cellSize = 72
          cellOffset = 38
          piece = translate (-200)(0) $ translate ((fromIntegral(x)-size)*cellSize + cellOffset) ((fromIntegral(y)-size)*cellSize + cellOffset) $
                case col of
                  White -> (p !! 5)
                  Black -> (p !! 4)

-- Calculates the next moves and then draws them in red on the board
drawHints :: Picture -> Int -> World -> Picture
drawHints p size w = pictures [mconcat hints]
        where
          tree = (buildTree (moveGenerator) (board w) (turn w))
          hints = map (\x -> drawHint p size (fst x)) (next_moves tree)

-- Draws an individual hint in the correct position
drawHint :: Picture -> Int -> Position-> Picture
drawHint p size' (x,y) = hint
       where
         cellSize = 72
         cellOffset = 38
         size = fromIntegral(size')/2
         hint = translate (-200)(0) $ translate ((fromIntegral(x)-size)*cellSize  +cellOffset) ((fromIntegral(y)-size)*cellSize + cellOffset) $ p

-- Draws the option menu and the current values of the board
drawOptions :: [Picture] -> World -> IO Picture
drawOptions p w = return optionPic
       where optionPic = pictures [p !! 6, screen, ai, start]
             screen = Translate (270) (-50) $ scale (0.4) (0.4) $ color blue $ text (show (size (board w)))
             ai = Translate (270) (-200) $ scale (0.4) (0.4) $ color blue $ text (show (ai_level w))
             start = if (random $ flags w)
                        then Translate (270) (-330) $ scale (0.4) (0.4) $ color blue $ text "Random"
                        else Translate (270) (-330) $ scale (0.4) (0.4) $ color blue $ text "Normal"
