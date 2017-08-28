module Main where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss

import System.Environment
import System.IO



import Board
import Draw
import Input
import AI

-- Parses the arguments given, loads the pictures used, makes the inital world and calles play and its given functions with these values
main :: IO ()
main = do args <- getArgs
          pictures <- loadPictures
          let flags = parseArguments args
          initWorld <- makeWorld flags
          playIO (InWindow "Othello" (1200, 800) (10, 10)) white 5
            initWorld -- in Board.hs
            (drawWorld pictures) -- in Draw.hs
            handleInput -- in Input.hs
            updateWorld -- in AI.hs

-- Parses the arguments into values for a flags data type
parseArguments :: [String] -> Flags
parseArguments args = foldl (\(Flags h e l s w r) str -> case str of
                                                  "-h" -> (Flags True False l s w r)
                                                  "-e" -> (Flags False True l s w r)
                                                  "-l" -> (Flags h e True False w r)
                                                  "-s" -> (Flags h e False True w r)
                                                  "-w" -> (Flags h e l s True r)
                                                  "-r" -> (Flags h e l s w True)
                                                  _    -> (Flags h e l s w r)) (Flags False False False False False False) args

-- Loads the pictures to be used in drawing the world
loadPictures :: IO [Picture]
loadPictures = do gameOver <- loadBMP "./Picture/GameOver.bmp"
                  background10 <- loadBMP "./Picture/10x10Board.bmp"
                  background8 <- loadBMP "./Picture/8x8Bitmap.bmp"
                  background6 <- loadBMP "./Picture/6x6Bitmap.bmp"
                  blackPiece <- loadBMP "./Picture/BlackPiece(10x10).bmp"
                  whitePiece <- loadBMP "./Picture/WhitePiece(10x10).bmp"
                  optionsMenu <- loadBMP "./Picture/Option_menu.bmp"
                  redPiece <- loadBMP "./Picture/RedPiece(10x10).bmp"
                  return [gameOver,background10,background8,background6,blackPiece,whitePiece, optionsMenu, redPiece]
