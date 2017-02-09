module MarsRover where

import Orientation
import Position
import Planet
import Control.Applicative
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as M

data Rover = Rover { position :: Position
                     , orientation :: Orientation
                     , blockedBy :: Maybe String
                   } deriving (Eq, Show, Read)

instance HasPosition Rover where
  getPosition = position
  setPosition (Rover _ o b) newPos = Rover newPos o b

type Mars = Planet Rover

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

moveRoverOnPlanet :: Planet Rover -> Rover -> Position -> (Rover, Planet Rover)
moveRoverOnPlanet (Planet size elmts) (Rover oldPos o s) newPos = (rover, Planet size reinserted)
  where rover = Rover newPos o s
        deleted = M.delete oldPos elmts
        reinserted = M.insert newPos (Plain (Just rover)) deleted

-- functor could improve this ?
applyCommand :: Mars -> Rover -> Command -> (Rover, Mars)
applyCommand planet (Rover p o s) (Left c) = (Rover p (changeOrientation c o) s, planet)
applyCommand planet@(Planet size elmts) rover@(Rover p o s) (Right c) =
  case elmtAtNextPos of Nothing -> moveRoverOnPlanet planet rover nextPos
                        Just Rock -> (Rover p o (Just ("Found rock at " ++ show nextPos)), planet)
                        Just (Plain a) -> (Rover p o (Just ("Found " ++ show a)), planet)
    where nextPos = constrainPosition size (move c o p)
          elmtAtNextPos = M.lookup nextPos elmts

applyCommands :: Mars -> Rover -> [Command] -> (Rover, Mars)
applyCommands currentPlanet currentRover [] = (currentRover, currentPlanet)
applyCommands currentPlanet currentRover commands =
  if isNothing(blockedBy rover)
    then applyCommands planet rover (tail commands)
    else (rover, planet)
  where (rover, planet) = applyCommand currentPlanet currentRover (head commands)

readCommands :: String -> [Command]
readCommands s = mapMaybe parseCommand (words s)

readRover :: String -> Rover
readRover s = read s :: Rover
