module MarsRover where

type Width = Int
type Height = Int

data Map = Map Width Height
data Position = Position Int Int deriving (Eq, Show)

data Orientation = North | East | South | West deriving (Eq, Show)

data Rover = Rover Position Orientation deriving (Eq, Show)

data Command = Forward | Backward | TurnLeft | TurnRight

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

applyCommand :: Rover -> Command -> Rover
applyCommand (Rover p o) TurnLeft = Rover p (changeOrientation TurnLeft o)
applyCommand (Rover p o) TurnRight = Rover p (changeOrientation TurnRight o)
applyCommand (Rover p o) Forward = Rover (move Forward o p) o
applyCommand (Rover p o) Backward = Rover (move Backward o p) o

applyCommands :: Rover -> [Command] -> Rover
applyCommands = foldl applyCommand

someFunc :: IO ()
someFunc = putStrLn "someFunc"
