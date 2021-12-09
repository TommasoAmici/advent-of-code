import Data.Char (digitToInt)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

type X = Int

type Y = Int

type Coordinate = (X, Y)

type Height = Int

type Risk = Int

type Basin = Set.Set Coordinate

type BasinSize = Int

type Vents = Map.Map Coordinate Height

readInput :: IO ([Coordinate], Vents)
readInput = do
  input <- readFile "input.txt"
  let split = lines input
  let coordinates = generateCoordinates (length split) (length (head split))
  let heights = concatMap (map digitToInt) split
  return (coordinates, Map.fromList (zip coordinates heights))

generateCoordinates :: X -> Y -> [Coordinate]
generateCoordinates xs ys = do
  x <- [0 .. xs -1]
  y <- [0 .. ys -1]
  return (x, y)

findDirections :: Coordinate -> [Coordinate]
findDirections (x, y) = [up, down, left, right]
  where
    up = (x, y -1)
    down = (x, y + 1)
    left = (x -1, y)
    right = (x + 1, y)

findHeightAllDirections :: Vents -> Coordinate -> [Height]
findHeightAllDirections vents (x, y) = map (fromMaybe 10) directionHeights
  where
    height = fromMaybe 9 (Map.lookup (x, y) vents)
    directions = findDirections (x, y)
    directionHeights = map (`Map.lookup` vents) directions

isLowPoint :: Vents -> Coordinate -> Bool
isLowPoint vents coord = all (height <) directions
  where
    height = fromMaybe 9 (Map.lookup coord vents)
    directions = findHeightAllDirections vents coord

findLowPoints :: Vents -> [Coordinate] -> [Coordinate]
findLowPoints vents coords = [c | c <- coords, isLowPoint vents c]

findRisk :: Vents -> Coordinate -> Risk
findRisk vents (x, y) = height + 1
  where
    height = fromMaybe 9 (Map.lookup (x, y) vents)

countLowPointsRisk :: [Coordinate] -> Vents -> Int
countLowPointsRisk lowpoints vents = sum risk
  where
    risk = map (findRisk vents) lowpoints

findBasin :: Vents -> Basin -> [Coordinate] -> Basin
findBasin vents basin [] = basin
findBasin vents basin (c : cs) = findBasin vents updatedBasin next
  where
    height = fromMaybe 9 (Map.lookup c vents)
    updatedBasin = if height < 9 then Set.insert c basin else basin
    allDirections = Set.fromList (if height < 9 then findDirections c else [])
    directionsToCheck = Set.difference allDirections updatedBasin
    next = Set.toList directionsToCheck ++ cs

findAllBasins :: Vents -> [Coordinate] -> [BasinSize]
findAllBasins vents = map (\c -> Set.size (findBasin vents Set.empty [c]))

findThreeBiggestBasins :: Vents -> [Coordinate] -> BasinSize
findThreeBiggestBasins vents lowpoints = product (take 3 (reverse (sort basinSizes)))
  where
    basinSizes = findAllBasins vents lowpoints

main :: IO ()
main = do
  (coordinates, vents) <- readInput
  let lowpoints = findLowPoints vents coordinates
  putStr "Part one: "
  print (countLowPointsRisk lowpoints vents)
  putStr "Part two: "
  print (findThreeBiggestBasins vents lowpoints)
