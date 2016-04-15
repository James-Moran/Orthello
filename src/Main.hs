module Main where

import Graphics.Gloss

import System.Environment
import System.Random
import System.IO



import Board
import Draw
import Input
import AI


-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move

main :: IO ()
main = do args <- getArgs
          let flags = parseArguments args
          random <- getRandomNumber flags
          let initWorld = makeWorld flags random
          play (InWindow "Othello" (640, 640) (10, 10)) black 10
            initWorld -- in Board.hs
            drawWorld -- in Draw.hs
            handleInput -- in Input.hs
            updateWorld -- in AI.hs

parseArguments :: [String] -> Flags
parseArguments args = foldl (\(Flags h e l s w r) str -> case str of
                                                  "-h" -> (Flags True False l s w r)
                                                  "-e" -> (Flags False True l s w r)
                                                  "-l" -> (Flags h e True False w r)
                                                  "-s" -> (Flags h e False True w r)
                                                  "-w" -> (Flags h e l s True r)
                                                  "-r" -> (Flags h e l s w True)
                                                  _    -> (Flags h e l s w r)) (Flags False False False False False False) args

getRandomNumber :: Flags -> IO (Int,Int)
getRandomNumber (Flags h e l s w r) = do
                                let size = if l then 10 else if s then 6 else 8
                                gen <- getStdGen
                                let (randomX, gen') = randomR (1, size-1) gen :: (Int, StdGen)
                                let (randomY, _) = randomR (1, size-1) gen' :: (Int, StdGen)
                                return (randomX, randomY)
