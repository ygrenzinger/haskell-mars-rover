import Test.Hspec
import Test.QuickCheck

import MarsRover

initPos = Position 1 1

width = 10
height = 10
mars = GroundMap width height

randomPositionOnMars :: Gen Position
randomPositionOnMars = do
  x <- elements [0..(width-1)]
  y <- elements [0..(height-1)]
  return (Position x y)

randomRoverOnMars :: Gen Rover
randomRoverOnMars = do
  position <- randomPositionOnMars
  orientation <- elements [North, South, West, East]
  return (Rover position orientation)

randomRoverAndMove :: Gen (Rover, Command)
randomRoverAndMove = do
  rover <- randomRoverOnMars
  move <- elements [Forward, Backward]
  return (rover, move)

prop_moveDontChangeOrientation :: Property
prop_moveDontChangeOrientation =
  forAll randomRoverAndMove
  (\(rover, move) -> getOrientation (applyCommand mars rover move) == getOrientation rover)

randomRoverAndTurn :: Gen (Rover, Command)
randomRoverAndTurn = do
  rover <- randomRoverOnMars
  turn <- elements [TurnLeft, TurnRight]
  return (rover, turn)

prop_turnDontChangePosition :: Property
prop_turnDontChangePosition =
  forAll randomRoverAndTurn
  (\(rover, turn) -> getPosition (applyCommand mars rover turn) == getPosition rover)

isRoverOnMars :: Rover -> Bool
isRoverOnMars rover = 0 <= x && x < 10 && 0 <= y && y < 10
  where (Position x y ) = getPosition rover

prop_shouldStayOnMars :: Property
prop_shouldStayOnMars =
  forAll randomRoverOnMars
  (\rover -> isRoverOnMars (applyCommands mars rover (replicate 20 Forward))
    && isRoverOnMars (applyCommands mars rover (replicate 20 Backward)))

main :: IO ()
main = hspec $ do
  describe "Change orientation" $ do
    it "should not change position"
      prop_turnDontChangePosition
    it  "turn left" $ do
      applyCommand mars (Rover initPos  North) TurnLeft `shouldBe` Rover (Position 1 1) West
      applyCommand mars (Rover initPos  South) TurnLeft `shouldBe` Rover (Position 1 1) East
    it  "turn right" $ do
      applyCommand mars (Rover initPos  North) TurnRight `shouldBe` Rover (Position 1 1) East
      applyCommand mars (Rover initPos  South) TurnRight `shouldBe` Rover (Position 1 1) West

  describe "Move" $ do
    it "should not change orientation"
      prop_moveDontChangeOrientation
    it  "forward" $ do
      applyCommand mars (Rover initPos  North) Forward `shouldBe` Rover (Position 1 2) North
      applyCommand mars (Rover initPos  South) Forward `shouldBe` Rover (Position 1 0) South
      applyCommand mars (Rover initPos  East) Forward `shouldBe` Rover (Position 2 1) East
      applyCommand mars (Rover initPos  West) Forward `shouldBe` Rover (Position 0 1) West
    it  "backward" $ do
      applyCommand mars (Rover initPos  South) Backward `shouldBe` Rover (Position 1 2) South
      applyCommand mars (Rover initPos  North) Backward `shouldBe` Rover (Position 1 0) North
      applyCommand mars (Rover initPos  West) Backward `shouldBe` Rover (Position 2 1) West
      applyCommand mars (Rover initPos  East) Backward `shouldBe` Rover (Position 0 1) East

  describe "Moving Rover" $ do
    it "shoud apply commands" $ do
      applyCommands mars (Rover initPos North) [Forward, TurnRight, Forward]`shouldBe` Rover (Position 2 2) East
    it "shoud stay on mars"
      prop_shouldStayOnMars
