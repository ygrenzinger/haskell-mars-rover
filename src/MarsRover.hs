module MarsRover where

import Orientation
import Position
import Control.Applicative
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as M

type Width = Int
type Height = Int

data Ground = Plain (Maybe Rover) | Rock deriving (Eq, Show, Read)
data PlanetSize = PlanetSize Width Height deriving (Eq, Show, Read)
type MapElements = Map Position Ground
data Planet = Planet PlanetSize MapElements deriving (Eq, Show, Read)

data Rover = Rover { getPosition :: Position
                     , getOrientation :: Orientation
                     , getError :: Maybe String
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

constrainPosition :: PlanetSize -> Position -> Position
constrainPosition (PlanetSize width height) (Position x y) = Position (x `mod` width) (y `mod` height)

moveRoverOnPlanet :: Planet -> Rover -> Position -> (Rover, Planet)
moveRoverOnPlanet (Planet size elmts) (Rover oldPos o s) newPos = (rover, Planet size reinserted)
  where rover = Rover newPos o s
        deleted = M.delete oldPos elmts
        reinserted = M.insert newPos (Plain (Just rover)) deleted

-- functor could improve this ?
applyCommand :: Planet -> Rover -> Command -> (Rover, Planet)
applyCommand planet (Rover p o s) (Left c) = (Rover p (changeOrientation c o) s, planet)
applyCommand planet@(Planet size elmts) rover@(Rover p o s) (Right c) =
  case elmtAtNextPos of Nothing -> moveRoverOnPlanet planet rover nextPos
                        Just Rock -> (Rover p o (Just ("Found rock at " ++ show nextPos)), planet)
                        Just (Plain a) -> (Rover p o (Just ("Found " ++ show a)), planet)
    where nextPos = constrainPosition size (move c o p)
          elmtAtNextPos = M.lookup nextPos elmts

applyCommands :: Planet -> Rover -> [Command] -> (Rover, Planet)
applyCommands currentPlanet currentRover [] = (currentRover, currentPlanet)
applyCommands currentPlanet currentRover commands =
  if isNothing(getError rover)
    then applyCommands planet rover (tail commands)
    else (rover, planet)
  where (rover, planet) = applyCommand currentPlanet currentRover (head commands)

readCommands :: String -> [Command]
readCommands s = mapMaybe parseCommand (words s)

readPlanetSize :: String -> PlanetSize
readPlanetSize s = read s :: PlanetSize

readRover :: String -> Rover
readRover s = read s :: Rover

readRocks :: String -> [Position]
readRocks s = map (uncurry Position) ps
  where ps = read s :: [(Int, Int)]

insertRockInMap :: MapElements -> Position -> MapElements
insertRockInMap m p = M.insert p Rock m

insertRocksInMap :: MapElements -> [Position] -> MapElements
insertRocksInMap = foldl insertRockInMap

buildMapElements :: Rover -> [Position] -> MapElements
buildMapElements rover rocks = roverInPosition
  where mapWithRocks = insertRocksInMap M.empty rocks
        roverInPosition = M.insert (getPosition rover) (Plain (Just rover)) mapWithRocks
