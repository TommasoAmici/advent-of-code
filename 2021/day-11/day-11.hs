import Data.Char (digitToInt)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map

type X = Int

type Y = Int

type Coordinate = (X, Y)

type Energy = Int

type Flashed = Bool

type Octopus = (Energy, Flashed)

type OctopusGrid = Map.Map Coordinate Octopus

type NumStep = Int

type FlashCount = Int

width :: X
width = 10

height :: Y
height = 10

readInput :: IO OctopusGrid
readInput = do
  input <- readFile "input.txt"
  return (parseInput input)

parseInput :: String -> OctopusGrid
parseInput input = Map.fromList (zip coords (zip energy (repeat False)))
  where
    energy = concatMap (map digitToInt) (lines input)

generateCoordinates :: X -> Y -> [Coordinate]
generateCoordinates xs ys = do
  x <- [0 .. xs -1]
  y <- [0 .. ys -1]
  return (x, y)

coords :: [Coordinate]
coords = generateCoordinates width height

isValidCoord :: Coordinate -> Bool
isValidCoord (x, y) = x >= 0 && x < width && y >= 0 && y < height

findNeighbors :: Coordinate -> [Coordinate]
findNeighbors (x, y) = filter (/= (x, y)) validCoordsIncludingSelf
  where
    validCoordsIncludingSelf =
      [ (x, y)
        | x <- [x -1 .. x + 1],
          y <- [y -1 .. y + 1],
          isValidCoord (x, y)
      ]

countIf :: Foldable f => (a -> Bool) -> f a -> Int
countIf cond = length . filter cond . toList

flash :: OctopusGrid -> OctopusGrid -> OctopusGrid
flash willFlash octopuses
  | Map.size willFlash == 0 = octopuses
  | otherwise = flash next increased
  where
    neighbors = concatMap findNeighbors (Map.keys willFlash)
    increased =
      Map.mapWithKey
        ( \k (energy, flashed) ->
            if not flashed && Map.member k willFlash
              then (0, True)
              else
                if not flashed && k `elem` neighbors
                  then (energy + countIf (== k) neighbors, flashed)
                  else (energy, flashed)
        )
        octopuses
    next = filterWillFlash increased

filterWillFlash :: OctopusGrid -> OctopusGrid
filterWillFlash = Map.filter (\(energy, flashed) -> not flashed && energy > 9)

step :: OctopusGrid -> OctopusGrid
step octopuses = afterFlash
  where
    -- increase all energy by 1, reset flashed
    increased = Map.map (\(energy, _) -> (energy + 1, False)) octopuses
    willFlash = filterWillFlash increased
    afterFlash = flash willFlash increased

hasFlashed :: Octopus -> Bool
hasFlashed = snd

countFlashes :: NumStep -> FlashCount -> OctopusGrid -> FlashCount
countFlashes 0 flashCount octopuses = flashCount
countFlashes n flashCount octopuses = countFlashes (n -1) (flashCount + numFlashes) afterStep
  where
    afterStep = step octopuses
    numFlashes = Map.size (Map.filter hasFlashed afterStep)

findSyncFlash :: NumStep -> OctopusGrid -> NumStep
findSyncFlash n octopuses = if allFlashed then n + 1 else findSyncFlash (n + 1) afterStep
  where
    afterStep = step octopuses
    allFlashed = Map.size afterStep == Map.size (Map.filter hasFlashed afterStep)

main :: IO ()
main = do
  octopuses <- readInput
  putStr "Part one: "
  print (countFlashes 100 0 octopuses)
  putStr "Part two: "
  print (findSyncFlash 0 octopuses)
