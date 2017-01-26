import Test.Hspec

import MarsRover

zeroPos = Position 0 0

main :: IO ()
main = hspec $ do
  describe "Change orientation" $ do
    it  "turn left" $ do
      applyCommand (Rover zeroPos  North) TurnLeft `shouldBe` Rover (Position 0 0) West
      applyCommand (Rover zeroPos  South) TurnLeft `shouldBe` Rover (Position 0 0) East
    it  "turn right" $ do
      applyCommand (Rover zeroPos  North) TurnRight `shouldBe` Rover (Position 0 0) East
      applyCommand (Rover zeroPos  South) TurnRight `shouldBe` Rover (Position 0 0) West

  describe "Move" $ do
    it  "forward" $ do
      applyCommand (Rover zeroPos  North) Forward `shouldBe` Rover (Position 0 1) North
      applyCommand (Rover zeroPos  South) Forward `shouldBe` Rover (Position 0 (-1)) South
      applyCommand (Rover zeroPos  East) Forward `shouldBe` Rover (Position 1 0) East
      applyCommand (Rover zeroPos  West) Forward `shouldBe` Rover (Position (-1) 0) West
    it  "backward" $ do
      applyCommand (Rover zeroPos  South) Backward `shouldBe` Rover (Position 0 1) South
      applyCommand (Rover zeroPos  North) Backward `shouldBe` Rover (Position 0 (-1)) North
      applyCommand (Rover zeroPos  West) Backward `shouldBe` Rover (Position 1 0) West
      applyCommand (Rover zeroPos  East) Backward `shouldBe` Rover (Position (-1) 0) East

  describe "Moving Rover" $ do
    it "shoud apply commands" $ do
      applyCommands (Rover zeroPos North) [Forward, TurnRight, Forward]`shouldBe` Rover (Position 1 1) East
