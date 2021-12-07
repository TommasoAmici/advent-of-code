{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import qualified Data.Map.Strict as MS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type X = Int

type Y = Int

newtype Coordinate = Coordinate (X, Y) deriving (Show, Eq, Ord)

type VentStart = Coordinate

type VentEnd = Coordinate

newtype Vent = Vent (VentStart, VentEnd) deriving (Show)

readInput :: IO [Vent]
readInput = do
  input <- TIO.readFile "input.txt"
  return (parseInput input)

parseInput :: T.Text -> [Vent]
parseInput input = [parseVent l | l <- T.lines input]

parseVent :: T.Text -> Vent
parseVent raw = Vent (start, end)
  where
    startEnd = T.splitOn " -> " raw
    start = parseCoordinate (head startEnd)
    end = parseCoordinate (startEnd !! 1)

parseCoordinate :: T.Text -> Coordinate
parseCoordinate raw = Coordinate (x, y)
  where
    xy = T.splitOn "," raw
    x = read (T.unpack (head xy))
    y = read (T.unpack (xy !! 1))

isNotDiagonal :: Vent -> Bool
isNotDiagonal (Vent (Coordinate (x1, y1), Coordinate (x2, y2))) = x1 == x2 || y1 == y2

coordInVent :: Vent -> [Coordinate]
coordInVent (Vent (Coordinate (x1, y1), Coordinate (x2, y2))) =
  [ Coordinate (x, y)
    | x <- [(minimum [x1, x2]) .. (maximum [x1, x2])],
      y <- [(minimum [y1, y2]) .. (maximum [y1, y2])]
  ]

coordInVentDiagonal :: Vent -> [Coordinate]
coordInVentDiagonal (Vent (Coordinate (x1, y1), Coordinate (x2, y2)))
  | x1 < x2 && y1 < y2 = [Coordinate x | x <- zip [x1 .. x2] [y1 .. y2]]
  | x1 > x2 && y1 < y2 = [Coordinate x | x <- zip (reverse [x2 .. x1]) [y1 .. y2]]
  | x1 < x2 && y1 > y2 = [Coordinate x | x <- zip [x1 .. x2] (reverse [y2 .. y1])]
  | x1 > x2 && y1 > y2 = [Coordinate x | x <- zip (reverse [x2 .. x1]) (reverse [y2 .. y1])]
  | otherwise = [] -- in theory you could have cases where x1 == x2 or y1 == y2

allCoordsWithVents :: [Vent] -> [Coordinate]
allCoordsWithVents vents = concat coords
  where
    coords =
      [ if isNotDiagonal vent
          then coordInVent vent
          else coordInVentDiagonal vent
        | vent <- vents
      ]

-- count coordinates that appear more than once
countCoords :: Map.Map Coordinate Int -> [Coordinate] -> Map.Map Coordinate Int
countCoords count [] = count
countCoords count (x : xs) = countCoords newCount xs
  where
    newCount = MS.insertWith (+) x 1 count

foldCount :: (Ord a1, Num a2, Num a1) => a1 -> a2 -> a2
foldCount count total = total + if count > 1 then 1 else 0

part1 :: [Vent] -> Int
part1 vents = MS.foldr foldCount 0 count
  where
    count = countCoords Map.empty (allCoordsWithVents (filter isNotDiagonal vents))

part2 :: [Vent] -> Int
part2 vents = MS.foldr foldCount 0 count
  where
    count = countCoords Map.empty (allCoordsWithVents vents)

main :: IO ()
main = do
  vents <- readInput
  putStr "Part one: "
  print (part1 vents)
  putStr "Part two: "
  print (part2 vents)
