module Block1.Task1Spec
  ( testTree
  ) where

import Block1.Task1

import Control.Monad (forM_)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, shouldNotBe, testSpec)


days :: [DayOfWeek]
days = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

testTree :: IO TestTree
testTree = testSpec "Task1" $ do

  describe "nextDay" $ do
      let withPrevDays = (Sunday : days) `zip` days  -- pairs of days in format (previos, current)
      forM_ withPrevDays $ \(prev, next) -> do
          it (show prev ++ " -> " ++ show next) $ do
              nextDay prev `shouldBe` next

  describe "afterDay" $ do
    it "Monday after 0 days" $ do
      afterDay 0 Monday `shouldBe` Monday
    it "Thursday after 3 days" $ do
      afterDay 3 Thursday `shouldBe` Sunday
    it "Saturday after 1 week" $ do
      afterDay 7 Saturday `shouldBe` Saturday
    it "Friday after 20 weeks" $ do
      afterDay (20 * 7) Friday `shouldBe` Friday

  describe "isWeekend" $ do
    it "Saturday should be weekend" $ do
      isWeekend Saturday `shouldBe` True
    it "Sunday should be weekend" $ do
      isWeekend Sunday `shouldBe` True
    let weekdays = take 5 days
    forM_ weekdays $ \weekday -> do
      it (show weekday ++ " shouldn't be weekend") $ do
        isWeekend weekday `shouldNotBe` True

  describe "daysToParty" $ do
    let daysFromSaturday = take 7 $ drop 5 $ cycle days  -- [Saturday, Sunday, ..., Friday]
    forM_ (daysFromSaturday `zip` [6, 5..1]) $ \(day, n) -> do
      it ("days to party from " ++ show day) $ do
        daysToParty day `shouldBe` (n :: Int)
