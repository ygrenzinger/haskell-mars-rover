module Position where

import Orientation
import Data.Monoid

data Position = Position Int Int deriving (Eq, Ord, Show, Read)

instance Monoid Position where
  mempty = Position 0 0
  mappend (Position xa ya) (Position xb yb) = Position (xa + xb) (ya + yb)

data PositionCommand = Forward | Backward deriving (Eq, Show, Read)

moves :: Orientation -> Position
moves North = Position 0 1
moves South = Position 0 (-1)
moves East = Position 1 0
moves West = Position (-1) 0

invertMove :: Position -> Position
invertMove (Position x y) = Position (-x) (-y)

move :: PositionCommand -> Orientation -> Position -> Position
move Forward o p = p <> moves o
move Backward o p = p <> invertMove (moves o)
