module Planet where

import Orientation
import Position
import Control.Applicative
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as M

type Width = Int
type Height = Int

data (HasPosition a) => Ground a = Plain (Maybe a) | Rock deriving (Eq, Show, Read)
data PlanetSize = PlanetSize Width Height deriving (Eq, Show, Read)
type MapElements a = Map Position (Ground a)
data Planet a = Planet PlanetSize (MapElements a) deriving (Eq, Show, Read)

constrainPosition :: PlanetSize -> Position -> Position
constrainPosition (PlanetSize width height) (Position x y) = Position (x `mod` width) (y `mod` height)

readPlanetSize :: String -> PlanetSize
readPlanetSize s = read s :: PlanetSize

readRocks :: String -> [Position]
readRocks s = map (uncurry Position) ps
  where ps = read s :: [(Int, Int)]

insertRockInMap :: (HasPosition a) => MapElements a -> Position -> MapElements a
insertRockInMap m p = M.insert p Rock m

insertRocksInMap :: (HasPosition a) => MapElements a -> [Position] -> MapElements a
insertRocksInMap = foldl insertRockInMap

buildMapElements :: (HasPosition a) => a -> [Position] -> MapElements a
buildMapElements elmt rocks = elmtInPosition
  where mapWithRocks = insertRocksInMap M.empty rocks
        elmtInPosition = M.insert (getPosition elmt) (Plain (Just elmt)) mapWithRocks
