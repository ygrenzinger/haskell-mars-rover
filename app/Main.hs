module Main where

import MarsRover
import Position
import Orientation

import System.IO
import Data.List
import Data.List.Split

cliPosition :: IO Position
cliPosition = do
  putStr "Which X position? "
  x <- readLn :: IO Int
  putStr "Which y position? "
  y <- readLn :: IO Int
  return (Position x y)

cliOrientation :: IO Orientation
cliOrientation = do
  putStr "Which orientation for rover? "
  readLn :: IO Orientation

cliRover :: IO Rover
cliRover = do
  position <- cliPosition
  orientation <- cliOrientation
  return (Rover position orientation)

cliGroundSize :: IO MapSize
cliGroundSize = do
  putStr "Which width for Mars? "
  width <- readLn :: IO Int
  putStr "Which height for Mars? "
  height <- readLn :: IO Int
  return (MapSize width height)

interactWithRover :: IO ()
interactWithRover = do
  mars <- cliGroundSize
  roverInput <- cliRover
  let startMessage  = "Rover " ++ show roverInput ++ " on " ++ show mars
  putStrLn startMessage
  putStrLn "Commands? "
  commandsInput <- getLine
  let rover = applyCommands mars roverInput $ readCommands commandsInput
  let endMessage = "Rover at " ++ show rover
  putStrLn endMessage

main :: IO ()
main = do
    fileContent <- readFile "mars-rover-input.txt"
    let contents = splitOn "\n" fileContent
    let mapSize = readMapSize $ head contents
    putStrLn ("MapSize : " ++ show mapSize)
    let rover = readRover $ contents !! 1
    putStrLn ("Rover start position : " ++ show rover)
    let rockPositions = readRocks $ contents !! 2
    putStrLn ("Rock positions : " ++ show rockPositions)
    let commands = readCommands $ contents !! 3
    putStrLn ("Commands : " ++ intercalate ", " (map printCommand commands))
    let roverEndState = applyCommands mapSize rover commands
    putStrLn ("Rover end position : " ++ show roverEndState)
