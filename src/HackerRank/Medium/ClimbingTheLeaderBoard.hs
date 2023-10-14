{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module HackerRank.Medium.ClimbingTheLeaderBoard where

-- https://www.hackerrank.com/challenges/climbing-the-leaderboard/problem

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
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

climbingLeaderboard :: [Int] -> [Int] -> [Int]
climbingLeaderboard ranked player = undefined
    -- Write your code here

lstrip :: String -> String
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack

rstrip :: String -> String
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
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

    let result = climbingLeaderboard ranked player

    hPutStrLn fptr $ Data.List.intercalate "\n" $ Data.List.map show result

    hFlush fptr
    hClose fptr
