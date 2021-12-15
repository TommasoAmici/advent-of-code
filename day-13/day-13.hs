import Data.Function (on)
import Data.List (nub, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type X = Int

type Y = Int

type Coordinate = (X, Y)

type Grid = [Coordinate]

type Direction = Char

type Fold = (Direction, Int)

generateCoordinates :: X -> Y -> [Coordinate]
generateCoordinates xs ys = do
  x <- [0 .. xs]
  y <- [0 .. ys]
  return (x, y)

sortCoordsByY :: [Coordinate] -> [Coordinate]
sortCoordsByY = sortBy (compare `on` snd)

readInput :: IO (Grid, [Fold])
readInput = do
  input <- readFile "input.txt"
  return (parseInput input)

parseFold :: String -> Fold
parseFold str = (dir, read (last split))
  where
    split = splitOn "=" str
    dir = last (head split)

parseCoords :: String -> Coordinate
parseCoords str = (read (head split), read (last split))
  where
    split = splitOn "," str

parseInput :: String -> (Grid, [Fold])
parseInput input = (grid, folds)
  where
    split = splitOn "\n\n" input
    grid = map parseCoords (lines (head split))
    folds = map parseFold (lines (last split))

visibleDots :: Grid -> Int
visibleDots grid = length (nub grid)

foldGrid' :: Grid -> Fold -> Grid
foldGrid' grid ('x', v) = map (\(x, y) -> if x > v then (x -2 * (x - v), y) else (x, y)) grid
foldGrid' grid ('y', v) = map (\(x, y) -> if y > v then (x, y -2 * (y - v)) else (x, y)) grid
foldGrid' grid (_, v) = grid

foldGrid :: Grid -> [Fold] -> Grid
foldGrid = foldl foldGrid'

showGrid :: Grid -> String
showGrid grid = str
  where
    dots = M.fromList (zip grid (repeat True))
    maxX = maximum (map fst grid)
    maxY = maximum (map snd grid)
    coords = sortCoordsByY (generateCoordinates maxX maxY)
    str =
      concatMap
        ( \(x, y) ->
            let c = if M.findWithDefault False (x, y) dots then "#" else " "
             in if x == maxX then c ++ "\n" else c
        )
        coords

main :: IO ()
main = do
  (grid, folds) <- readInput
  putStr "Part one: "
  print (visibleDots (foldGrid' grid (head folds)))
  putStrLn "Part two: "
  putStrLn (showGrid (foldGrid grid folds))
