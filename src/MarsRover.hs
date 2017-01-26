module MarsRover where

import Data.Monoid

type Width = Int
type Height = Int

data GroundMap = GroundMap Width Height
data Position = Position Int Int deriving (Eq, Show)

instance Monoid Position where
  mempty = Position 0 0
  mappend (Position xa ya) (Position xb yb) = Position (xa + xb) (ya + yb)

data Orientation = North | East | South | West deriving (Eq, Show)

data Rover = Rover { getPosition :: Position
                     , getOrientation :: Orientation
                     } deriving (Eq, Show)

data Command = Forward | Backward | TurnLeft | TurnRight deriving (Eq, Show)

constrainPosition :: GroundMap -> Position -> Position
constrainPosition (GroundMap width height) (Position x y) = Position (x `mod` width) (y `mod` height)

changeOrientation :: Command -> Orientation -> Orientation
changeOrientation TurnLeft North = West
changeOrientation TurnLeft West = South
changeOrientation TurnLeft South = East
changeOrientation TurnLeft East = North
changeOrientation TurnRight North = East
changeOrientation TurnRight East = South
changeOrientation TurnRight South = West
changeOrientation TurnRight West = North
changeOrientation _ o = o

move :: Command  -> Orientation -> Position -> Position
move Forward North (Position x y) = Position x (y+1)
move Forward East (Position x y) = Position (x+1) y
move Forward South (Position x y) = Position x (y-1)
move Forward West (Position x y) = Position (x-1) y
move Backward North (Position x y) = Position x (y-1)
move Backward East (Position x y) = Position (x-1) y
move Backward South (Position x y) = Position x (y+1)
move Backward West (Position x y) = Position (x+1) y
move _ _ p = p

applyCommand :: GroundMap -> Rover -> Command -> Rover
applyCommand m (Rover p o) TurnLeft = Rover p (changeOrientation TurnLeft o)
applyCommand m (Rover p o) TurnRight = Rover p (changeOrientation TurnRight o)
applyCommand m (Rover p o) Forward = Rover (constrainPosition m (move Forward o p)) o
applyCommand m (Rover p o) Backward = Rover (constrainPosition m (move Backward o p)) o

applyCommands :: GroundMap -> Rover -> [Command] -> Rover
applyCommands m = foldl (applyCommand m)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
