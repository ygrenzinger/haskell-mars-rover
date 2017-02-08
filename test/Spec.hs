import Test.Hspec
import Test.QuickCheck

import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as M

import MarsRover
import Position
import Orientation

initPos = Position 1 1
initRover = Rover initPos North Nothing

width = 10
height = 10
size = PlanetSize width height

buildPlanet :: Rover -> [Position] -> Planet
buildPlanet rover@(Rover pos o s) rocks = Planet size withRocks
  where elmts = M.insert pos (Plain (Just rover)) M.empty
        withRocks = insertRocksInMap elmts rocks

planet = buildPlanet initRover []

randomPositionOnplanet :: Gen Position
randomPositionOnplanet = do
  x <- elements [0..(width-1)]
  y <- elements [0..(height-1)]
  return (Position x y)

randomRoverOnplanet :: Gen Rover
randomRoverOnplanet = do
  position <- randomPositionOnplanet
  orientation <- elements [North, South, West, East]
  return (Rover position orientation Nothing)

randomRoverAndMove :: Gen (Rover, Command)
randomRoverAndMove = do
  rover <- randomRoverOnplanet
  move <- elements [Right Forward, Right Backward]
  return (rover, move)

prop_moveDontChangeOrientation :: Property
prop_moveDontChangeOrientation =
  forAll randomRoverAndMove
  (\(rover, move) -> getOrientation (fst (applyCommand planet rover move)) == getOrientation rover)

randomRoverAndTurn :: Gen (Rover, Command)
randomRoverAndTurn = do
  rover <- randomRoverOnplanet
  turn <- elements [Left TurnLeft, Left TurnRight]
  return (rover, turn)

prop_turnDontChangePosition :: Property
prop_turnDontChangePosition =
  forAll randomRoverAndTurn
  (\(rover, turn) -> getPosition (fst (applyCommand planet rover turn)) == getPosition rover)

isRoverOnplanet :: Rover -> Bool
isRoverOnplanet rover = 0 <= x && x < width && 0 <= y && y < height
  where (Position x y ) = getPosition rover

prop_shouldStayOnplanet :: Property
prop_shouldStayOnplanet =
  forAll randomRoverOnplanet
  (\rover -> isRoverOnplanet (fst (applyCommands planet rover (replicate 20 (Right Forward))))
    && isRoverOnplanet (fst (applyCommands planet rover (replicate 20 (Right Backward)))))

main :: IO ()
main = hspec $ do
  describe "Change orientation" $ do
    it "should not change position"
      prop_turnDontChangePosition
    it  "turn left" $ do
      fst (applyCommand planet (Rover initPos North Nothing) (Left TurnLeft)) `shouldBe` Rover (Position 1 1) West Nothing
      fst (applyCommand planet (Rover initPos South Nothing) (Left TurnLeft)) `shouldBe` Rover (Position 1 1) East Nothing
    it  "turn right" $ do
      fst (applyCommand planet (Rover initPos North Nothing) (Left TurnRight)) `shouldBe` Rover (Position 1 1) East Nothing
      fst (applyCommand planet (Rover initPos South Nothing) (Left TurnRight)) `shouldBe` Rover (Position 1 1) West Nothing

  describe "Move" $ do
    it "should not change orientation"
      prop_moveDontChangeOrientation
    it  "forward" $ do
      fst (applyCommand planet (Rover initPos  North Nothing) (Right Forward)) `shouldBe` Rover (Position 1 2) North Nothing
      fst (applyCommand planet (Rover initPos  South Nothing) (Right Forward)) `shouldBe` Rover (Position 1 0) South Nothing
      fst (applyCommand planet (Rover initPos  East Nothing) (Right Forward)) `shouldBe` Rover (Position 2 1) East Nothing
      fst (applyCommand planet (Rover initPos  West Nothing) (Right Forward)) `shouldBe` Rover (Position 0 1) West Nothing
    it  "backward" $ do
      fst (applyCommand planet (Rover initPos  South Nothing) (Right Backward)) `shouldBe` Rover (Position 1 2) South Nothing
      fst (applyCommand planet (Rover initPos  North Nothing) (Right Backward)) `shouldBe` Rover (Position 1 0) North Nothing
      fst (applyCommand planet (Rover initPos  West Nothing) (Right Backward)) `shouldBe` Rover (Position 2 1) West Nothing
      fst (applyCommand planet (Rover initPos  East Nothing) (Right Backward)) `shouldBe` Rover (Position 0 1) East Nothing

  describe "Applying commands on rover" $
    it "shoud apply commands" $
      fst (applyCommands planet (Rover initPos North Nothing) [Right Forward, Left TurnRight, Right Forward]) `shouldBe` Rover (Position 2 2) East Nothing

  describe "Managing planet" $ do
    it "shoud stay on planet"
      prop_shouldStayOnplanet
    it "should move rover on planet" $
      snd (applyCommand planet initRover (Right Forward)) `shouldBe` buildPlanet (Rover (Position 1 2) North Nothing) []
    it "should stop rover if rocks, indicate error and don't change planet" $ do
      let planetWithRocks = buildPlanet initRover [Position 1 2]
      let roverBlocked = applyCommands planetWithRocks initRover [Right Forward]
      fst roverBlocked `shouldBe` Rover (Position 1 1) North (Just "Found rock at Position 1 2")
      snd roverBlocked `shouldBe` planetWithRocks
