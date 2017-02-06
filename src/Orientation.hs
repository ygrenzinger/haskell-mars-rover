module Orientation where

import Data.Maybe

data Orientation = North | East | South | West
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data OrientationCommand = TurnLeft | TurnRight
  deriving (Eq, Show, Read)

changeOrientation :: OrientationCommand -> Orientation -> Orientation
changeOrientation TurnLeft North = West
changeOrientation TurnRight West = North
changeOrientation TurnLeft o = pred o
changeOrientation TurnRight o = succ o
