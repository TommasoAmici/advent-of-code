{-# LANGUAGE OverloadedStrings #-}

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

type DaysLeft = Int

type DaysToBreed = Int

newtype Fish = Fish (DaysToBreed, DaysLeft) deriving (Show, Eq, Ord)

totalDaysPart1 :: DaysLeft
totalDaysPart1 = 80

readInput :: IO [DaysToBreed]
readInput = do
  input <- readFile "input.txt"
  return $ map read $ splitOn "," input

-- recursively breed fish, naive solution that chokes on part 2
fishGrowth :: Fish -> [Fish]
fishGrowth (Fish (daysToBreed, 0)) = [Fish (daysToBreed, 0)]
fishGrowth (Fish (0, daysLeft)) = concatMap fishGrowth [Fish (6, daysLeft -1), Fish (8, daysLeft -1)]
fishGrowth (Fish (daysToBreed, daysLeft)) =
  if daysLeft > daysToBreed
    then fishGrowth (Fish (0, daysLeft - daysToBreed))
    else fishGrowth (Fish (daysToBreed - daysLeft, 0))

fishGrowthPerStartingDay :: DaysLeft -> DaysToBreed -> (DaysToBreed, Int)
fishGrowthPerStartingDay daysLeft daysToBreed = (daysToBreed, length (fishGrowth (Fish (daysToBreed, daysLeft))))

growthPerStartingDay :: DaysLeft -> [DaysToBreed] -> Map.Map DaysToBreed Int
growthPerStartingDay daysLeft uniqueStartingDays = Map.fromList (map (fishGrowthPerStartingDay daysLeft) uniqueStartingDays)

calcFinalFish :: DaysLeft -> IO ()
calcFinalFish daysLeft = do
  startingDays <- readInput
  let uniqueStartingDays = nub startingDays
  let growthMap = growthPerStartingDay daysLeft uniqueStartingDays
  let result = sum $ sum <$> map (`Map.lookup` growthMap) startingDays
  print result

main :: IO ()
main = do
  putStr "Part one: "
  calcFinalFish totalDaysPart1
