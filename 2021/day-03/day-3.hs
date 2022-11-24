{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readValues :: IO [T.Text]
readValues = do
  input <- TIO.readFile "input.txt"
  let values = T.lines input
  return values

countOnes :: [T.Text] -> [Int]
countOnes = map (T.count "1")

divide :: Int -> Int -> Double
divide x y = fromIntegral x / fromIntegral y

binTextToDec :: T.Text -> Int
binTextToDec = T.foldl' (\acc x -> acc * 2 + (\x -> if x == '1' then 1 else 0) x) 0

mostCommon :: [T.Text] -> Int -> [Char]
mostCommon transposedValues numValues = map (\x -> if divide x numValues >= 0.5 then '1' else '0') (countOnes transposedValues)

leastCommon :: [T.Text] -> Int -> [Char]
leastCommon transposedValues numValues = map (\x -> if divide x numValues < 0.5 then '1' else '0') (countOnes transposedValues)

part1 :: [T.Text] -> Int
part1 values = binTextToDec (T.pack gamma) * binTextToDec (T.pack epsilon)
  where
    transposed = T.transpose values
    gamma = mostCommon transposed (length values)
    epsilon = leastCommon transposed (length values)

filterCommon :: T.Text -> [Char] -> Int -> Bool
filterCommon val common index = T.index val index == common !! index

findReading :: ([T.Text] -> Int -> [Char]) -> Int -> [T.Text] -> Int
findReading commonFunc _ [x] = binTextToDec x
findReading commonFunc index values = findReading commonFunc (index + 1) filtered
  where
    transposed = T.transpose values
    common = commonFunc transposed (length values)
    filtered = filter (\val -> filterCommon val common index) values

oxygen :: Int -> [T.Text] -> Int
oxygen = findReading mostCommon

co2Scrubber :: Int -> [T.Text] -> Int
co2Scrubber = findReading leastCommon

part2 :: [T.Text] -> Int
part2 values = ox * co2
  where
    transposed = T.transpose values
    ox = oxygen 0 values
    co2 = co2Scrubber 0 values

main :: IO ()
main = do
  values <- readValues
  putStr "Part one: "
  print (part1 values)
  putStr "Part two: "
  print (part2 values)
