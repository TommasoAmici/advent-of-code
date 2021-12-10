import Control.Monad

type Depth = Int

type Direction = String -- "forward" | "down" | "up"

type Command = (Direction, Depth)

type CoordX = Int

type CoordY = Int

type Aim = Int

type Coordinates = (CoordX, CoordY)

origin :: Coordinates
origin = (0, 0)

readCommands :: IO [Command]
readCommands = do
  input <- readFile "input.txt"
  let rawCommands = lines input
  let commands = parseCommands rawCommands
  return commands

parseCommands :: Monad m => m String -> m Command
parseCommands rawCommands = do
  raw <- rawCommands
  let split = words raw
  return (head split, read (split !! 1))

-- part 1
calcCoordinatesMovement1 :: Coordinates -> [Command] -> Coordinates
calcCoordinatesMovement1 (startX, startY) [] = (startX, startY)
calcCoordinatesMovement1 (startX, startY) (x : xs)
  | fst x == "forward" = calcCoordinatesMovement1 (startX + snd x, startY) xs
  | fst x == "up" = calcCoordinatesMovement1 (startX, startY - snd x) xs
  | fst x == "down" = calcCoordinatesMovement1 (startX, startY + snd x) xs
  | otherwise = calcCoordinatesMovement1 (startX, startY) xs

-- part 2
initialAim :: Aim
initialAim = 0

calcCoordinatesMovement2 :: Coordinates -> Aim -> [Command] -> Coordinates
calcCoordinatesMovement2 (startX, startY) _ [] = (startX, startY)
calcCoordinatesMovement2 (startX, startY) aim (x : xs)
  | fst x == "forward" = calcCoordinatesMovement2 (startX + snd x, startY + (aim * snd x)) aim xs
  | fst x == "up" = calcCoordinatesMovement2 (startX, startY) (aim - snd x) xs
  | fst x == "down" = calcCoordinatesMovement2 (startX, startY) (aim + snd x) xs
  | otherwise = calcCoordinatesMovement2 (startX, startY) aim xs

main :: IO ()
main = do
  commands <- readCommands
  let (endX1, endY1) = calcCoordinatesMovement1 origin commands
  print (endX1 * endY1)
  let (endX2, endY2) = calcCoordinatesMovement2 origin initialAim commands
  print (endX2 * endY2)
