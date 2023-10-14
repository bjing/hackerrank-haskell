module Spec.HackerRank.Medium.ClimbingTheLeaderboardSpec where

import HackerRank.Medium.ClimbingTheLeaderBoard
import Test.Hspec
import Control.Monad.State
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

spec :: Spec
spec = do

  describe "Climbing the Leaderboard" $ do
    let scores = [8, 6, 5, 3, 2, 1]
    let leaderboard = [(1, 8), (2, 6), (3, 5), (4, 3), (5, 2), (6, 1)]

    describe "utility functions" $ do
      it "should generate leaderboard from scores" $ do
        let lb = genLBFromScores scores
        lb `shouldBe` leaderboard
      it "should get scores from leaderboard" $ do
        let scores = getScoresFromLB leaderboard
        scores `shouldBe` scores
      it "should look up score from leaderboard" $ do
        lookupFromLeaderboard 1 leaderboard `shouldBe` Just 6
        lookupFromLeaderboard 3 leaderboard `shouldBe` Just 4
        lookupFromLeaderboard 5 leaderboard `shouldBe` Just 3
      it "should insert a score into ordered score list" $ do
        insertInOrderedList 6 scores `shouldBe` scores
        insertInOrderedList 8 scores `shouldBe` scores
        insertInOrderedList 1 scores `shouldBe` scores
        insertInOrderedList 4 scores `shouldBe` [8, 6, 5, 4, 3, 2, 1]
        insertInOrderedList 4 [] `shouldBe` [4]

    describe "the leader board" $ do
      it "should insert score into leaderboard and get back its rank" $ do
        let lbScores = [100, 100, 50, 40, 40, 20, 10]
        let lb = genLBFromScores $ dedupLBScores lbScores
        let playerScores = [5, 25, 50, 120]

        (result, _) <- liftIO $ runStateT (mapM insertIntoLeaderboard playerScores) lb
        (Maybe.catMaybes result) `shouldBe` [6, 4, 2, 1]

    describe "test against real dataset" $ do
      it "should pass test case 6" $ do
        pending

      it "should pass test case 7" $ do
        pending

      it "should pass test case 8" $ do
        pending

      it "should pass test case 9" $ do
        pending
