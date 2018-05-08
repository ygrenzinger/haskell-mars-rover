module Orientation where

data Orientation = North | East | South | West
  deriving (Eq, Show, Read, Enum)

data OrientationCommand = TurnLeft | TurnRight
  deriving (Eq, Show, Read)

changeOrientation :: OrientationCommand -> Orientation -> Orientation
changeOrientation TurnLeft North = West
changeOrientation TurnRight West = North
changeOrientation TurnLeft o = pred o
changeOrientation TurnRight o = succ o
