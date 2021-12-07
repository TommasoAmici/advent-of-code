{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split (splitOn)

type DaysLeft = Int

type DaysToBreed = Int

type Fish = Int

totalDaysPart1 :: DaysLeft
totalDaysPart1 = 80

totalDaysPart2 :: DaysLeft
totalDaysPart2 = 256

readInput :: IO [DaysToBreed]
readInput = do
  input <- readFile "input.txt"
  return $ map read $ splitOn "," input

fishGrowth :: (Fish, Fish, Fish, Fish, Fish, Fish, Fish, Fish, Fish) -> DaysLeft -> Int
fishGrowth (f0, f1, f2, f3, f4, f5, f6, f7, f8) 0 = f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8
fishGrowth (f0, f1, f2, f3, f4, f5, f6, f7, f8) n = fishGrowth (f1, f2, f3, f4, f5, f6, f0 + f7, f8, f0) (n -1)

countElInList :: DaysToBreed -> [DaysToBreed] -> Fish
countElInList day initial = length (filter (== day) initial)

groupInitialFish :: [DaysToBreed] -> (Fish, Fish, Fish, Fish, Fish, Fish, Fish, Fish, Fish)
groupInitialFish initial =
  ( countElInList 0 initial,
    countElInList 1 initial,
    countElInList 2 initial,
    countElInList 3 initial,
    countElInList 4 initial,
    countElInList 5 initial,
    countElInList 6 initial,
    countElInList 7 initial,
    countElInList 8 initial
  )

calcFinalFish :: DaysLeft -> IO ()
calcFinalFish daysLeft = do
  initial <- readInput
  let fish = groupInitialFish initial
  print (fishGrowth fish daysLeft)

main :: IO ()
main = do
  putStr "Part one: "
  calcFinalFish totalDaysPart1
  putStr "Part two: "
  calcFinalFish totalDaysPart2
