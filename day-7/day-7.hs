{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable (foldl')
import Data.List (sort)
import Data.List.Split (splitOn)

type Position = Int

type Fuel = Int

readInput :: IO [Position]
readInput = do
  input <- readFile "input.txt"
  return (map read (splitOn "," input))

mean :: [Position] -> Position
mean positions = foldl' (+) 0 positions `div` length positions

median :: [Position] -> Position
median positions = sort positions !! (length positions `div` 2)

-- each movement costs 1 fuel
fuelConsumption :: Position -> [Position] -> Fuel
fuelConsumption toReach positions = foldl' (+) 0 [abs (pos - toReach) | pos <- positions]

-- each movement costs 1 fuel more than the previous movement
fuelConsumption2 :: Position -> [Position] -> Fuel
fuelConsumption2 toReach positions = foldl' (+) 0 ([foldl' (+) 0 [1 .. abs (pos - toReach)] | pos <- positions])

alignCrabsToMedian :: [Position] -> Fuel
alignCrabsToMedian positions = fuelConsumption (median positions) positions

alignCrabsToMean :: [Position] -> Fuel
alignCrabsToMean positions = fuelConsumption2 (mean positions) positions

main :: IO ()
main = do
  crabs <- readInput
  putStr "Part one: "
  print (alignCrabsToMedian crabs)
  putStr "Part two: "
  print (alignCrabsToMean crabs)
