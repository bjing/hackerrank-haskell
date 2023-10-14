{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances, BlockArguments #-}

module HackerRank.Medium.ClimbingTheLeaderBoard where

-- https://www.hackerrank.com/challenges/climbing-the-leaderboard/problem

import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import qualified Data.Maybe as Maybe
import Data.Ord
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'climbingLeaderboard' function below.
--
-- The function is expected to return an INTEGER_ARRAY.
-- The function accepts following parameters:
--  1. INTEGER_ARRAY ranked
--  2. INTEGER_ARRAY player
--
type Rank = Int
type PlayerScore = Int
type Leaderboard = [(Rank, PlayerScore)]
type LeaderboardStore a = StateT Leaderboard IO a  -- this is a monad

lookupFromLeaderboard :: Int -> [(Int, Int)] -> Maybe Int
lookupFromLeaderboard score = fmap fst . Data.List.find (\(idx, s) -> s == score)

getScoresFromLB :: Leaderboard -> [PlayerScore]
getScoresFromLB = Data.List.map snd

genLBFromScores :: [PlayerScore] -> Leaderboard
genLBFromScores scores = Data.List.zip [1..Data.List.length scores] scores

lookupScoreFromLB :: PlayerScore -> Leaderboard -> Maybe Rank
lookupScoreFromLB score = fmap fst . Data.List.find ((== score). snd)

-- dedup leaderboard scores and make sure it's sorted
dedupLBScores :: [Int] -> [Int]
dedupLBScores = sortBy (comparing Down) . Data.Set.toList . Data.Set.fromList

insertInOrderedList :: Int -> [Int] -> [Int]
insertInOrderedList a [] = [a]
insertInOrderedList a l@(x:xs)
  | a < x = x : insertInOrderedList a xs
  | a == x = l
  | otherwise = a:l

-- Insert a player score to the leaderboard and get back its rank
insertIntoLeaderboard :: PlayerScore -> LeaderboardStore (Maybe Rank)
insertIntoLeaderboard p = do
  lb <- get
  let scores = getScoresFromLB lb
  liftIO $ putStrLn $ "Scores before update: " ++ show scores
  let combinedScores = insertInOrderedList p scores
  liftIO $ putStrLn $ "Updated Leaderboard scores: " ++ show combinedScores
  let updatedLB = genLBFromScores combinedScores
  _ <- put updatedLB
  pure $ lookupScoreFromLB p updatedLB

climbingLeaderboard :: [Rank] -> [PlayerScore] -> IO [Rank]
climbingLeaderboard ranked player = do
  let dedupedLB = dedupLBScores ranked
      computation = mapM insertIntoLeaderboard player
  (result, _) <- runStateT computation (genLBFromScores dedupedLB)
  pure $ Maybe.catMaybes result

lstrip :: String -> String
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack

rstrip :: String -> String
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO ()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    rankedCountTemp <- getLine
    let rankedCount = read $ lstrip $ rstrip rankedCountTemp :: Int

    rankedTemp <- getLine

    let ranked = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip rankedTemp

    playerCountTemp <- getLine
    let playerCount = read $ lstrip $ rstrip playerCountTemp :: Int

    playerTemp <- getLine

    let player = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip playerTemp

    result <- climbingLeaderboard ranked player

    hPutStrLn fptr $ Data.List.intercalate "\n" $ Data.List.map show result

    hFlush fptr
    hClose fptr
