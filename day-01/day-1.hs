main :: IO ()
main = do
  depths <- readDepths
  -- part 1
  let increases = depthIncreases depths
  let totalIncreases = sumBools increases
  print totalIncreases
  -- part 2
  let windows = sumsSlidingWindow depths
  let windowIncreases = depthIncreases windows
  let totalWindowIncreases = sumBools windowIncreases
  print totalWindowIncreases

type Depth = Int

readDepths :: IO [Depth]
readDepths = do
  input <- readFile "input.txt"
  let depths = read <$> lines input :: [Depth]
  return depths

sumBools :: [Bool] -> Int
sumBools bools = sum (map (\x -> if x then 1 else 0) bools)

-- part 1, measure increase from previous measure
depthIncreases :: [Depth] -> [Bool]
depthIncreases depths = do
  change <- depthChanges depths
  let increased = snd change > fst change
  return increased

depthChanges :: [Depth] -> [(Depth, Depth)]
depthChanges depths = zip depths depthsOffset
  where
    depthsOffset = mconcat [tail depths, [0]]

-- part 2, measure increase from previous sliding window
sumsSlidingWindow :: [Depth] -> [Depth]
sumsSlidingWindow depths = map sum windowed
  where
    windowed = slidingWindow depths

windowSize :: Int
windowSize = 3

slidingWindow :: [Depth] -> [[Depth]]
slidingWindow [] = []
slidingWindow depths = window : slidingWindow next
  where
    window = take windowSize depths
    next = tail depths
