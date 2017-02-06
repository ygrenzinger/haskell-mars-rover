module MarsRover where

import Orientation
import Position
import Control.Applicative
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as M

type Width = Int
type Height = Int

data Ground = Plain (Maybe Rover) | Rock
data MapSize = MapSize Width Height deriving (Eq, Show, Read)
type MapElements = Map Position Ground

data Rover = Rover { getPosition :: Position
                     , getOrientation :: Orientation
                     } deriving (Eq, Show, Read)

type Command = Either OrientationCommand PositionCommand

printCommand :: Command -> String
printCommand (Left TurnLeft) = "Turning left"
printCommand (Left TurnRight) = "Turning right"
printCommand (Right Forward) = "Going forward"
printCommand (Right Backward) = "Going backward"


parseCommand :: String -> Maybe Command
parseCommand "f" = Just (Right Forward)
parseCommand "b" = Just (Right Backward)
parseCommand "l" = Just (Left TurnLeft)
parseCommand "r" = Just (Left TurnRight)
parseCommand _ = Nothing

constrainPosition :: MapSize -> Position -> Position
constrainPosition (MapSize width height) (Position x y) = Position (x `mod` width) (y `mod` height)

applyCommand :: MapSize -> Rover -> Command -> Rover
applyCommand m (Rover p o) (Left c) = Rover p (changeOrientation c o)
applyCommand m (Rover p o) (Right c) = Rover (constrainPosition m (move c o p)) o

applyCommands :: MapSize -> Rover -> [Command] -> Rover
applyCommands m = foldl (applyCommand m)

readCommands :: String -> [Command]
readCommands s = mapMaybe parseCommand (words s)

readMapSize :: String -> MapSize
readMapSize s = read s :: MapSize

readRover :: String -> Rover
readRover s = read s :: Rover

readRocks :: String -> [Position]
readRocks s = map (uncurry Position) ps
  where ps = read s :: [(Int, Int)]

insertRockInMap :: MapElements -> Position -> MapElements
insertRockInMap m p = M.insert p Rock m

buildMapElements :: Rover -> [Position] -> MapElements
buildMapElements rover rocks = roverInPosition
  where mapWithRocks = foldl insertRockInMap M.empty rocks
        roverInPosition = M.insert (getPosition rover) (Plain (Just rover)) mapWithRocks
