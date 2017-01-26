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

moves :: Orientation -> Position
moves North = Position 0 1
moves South = Position 0 (-1)
moves East = Position 1 0
moves West = Position (-1) 0

invertMove :: Position -> Position
invertMove (Position x y) = Position (-x) (-y)

move :: Command  -> Orientation -> Position -> Position
move Forward o p = p <> moves o
move Backward o p = p <> invertMove (moves o)
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
