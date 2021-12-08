import Data.Foldable (Foldable (foldl'))
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

type Segment = Char

type Signal = [Segment]

type ConvertedSignal = Int

type Display = ([Signal], [Signal])

type ConvertedDisplay = ([ConvertedSignal], [ConvertedSignal])

readInput :: IO [Display]
readInput = do
  input <- readFile "input.txt"
  let displays = lines input
  return (parseDisplay displays)

parseDisplay :: Monad m => m String -> m Display
parseDisplay displays = do
  display <- displays
  let inputOutput = splitOn "|" display
  let input = head inputOutput
  let output = head (tail inputOutput)
  let inputSignals = words input :: [Signal]
  let oututSignals = words output :: [Signal]
  return (inputSignals, oututSignals)

countEasyDigits :: Monad m => m Signal -> m ConvertedSignal
countEasyDigits signals = do
  signal <- signals
  let len = length signal
  let converted
        | len == 2 = 1 -- 1
        | len == 3 = 1 -- 7
        | len == 4 = 1 -- 4
        | len == 7 = 1 -- 8
        | otherwise = 0
  return converted

part1 :: (Foldable t, Monad t) => t Display -> Int
part1 displays = foldl' (+) 0 $ countEasyDigits output
  where
    output = concatMap snd displays

filterBySetSize :: Int -> [Set.Set a] -> [Set.Set a]
filterBySetSize n = filter (\d -> Set.size d == n)

-- Returns segment by excluding a and b from the given set
findExcludingFromSet :: Int -> [Set.Set Segment] -> Set.Set Segment -> Set.Set Segment -> Set.Set Segment
findExcludingFromSet size digits a b = head $ filter (\s -> s /= a && s /= b) filtered
  where
    filtered = filterBySetSize size digits

findSubsetOfDifference :: Int -> [Set.Set Segment] -> Set.Set Segment -> Set.Set Segment -> Set.Set Segment
findSubsetOfDifference size digits a b = head $ filter (Set.isSubsetOf subset) filtered
  where
    filtered = filterBySetSize size digits
    subset = Set.difference a b

findNine :: [Set.Set Segment] -> Set.Set Segment -> Set.Set Segment -> Set.Set Segment -> Set.Set Segment
findNine digits eight seven four = head $ filter (\x -> Set.isSubsetOf nine_ x && x /= eight) digits
  where
    nine_ = Set.union seven four

findSix :: [Set.Set Segment] -> Set.Set Segment -> Set.Set Segment
findSix digits one = head six
  where
    sizeSix = filterBySetSize 6 digits
    six = filter (not . Set.isSubsetOf one) sizeSix

-- transform [1,2,3,4] into 1234
positionalNotation :: [Int] -> Int
positionalNotation xs = foldl' (\y (x, e) -> y + x * (10 ^ e)) 0 nums
  where
    exponents = reverse [0 .. length xs - 1]
    nums = zip xs exponents

convertDisplay :: Monad m => m Display -> m ConvertedSignal
convertDisplay displays = do
  display <- displays
  let sets = map Set.fromList $ fst display
  let one = head $ filter (\x -> Set.size x == 2) sets
  let seven = head $ filter (\x -> Set.size x == 3) sets
  let four = head $ filter (\x -> Set.size x == 4) sets
  let eight = head $ filter (\x -> Set.size x == 7) sets
  let nine = findNine sets eight seven four
  let six = findSix sets one
  let zero = findExcludingFromSet 6 sets six nine
  let two = findSubsetOfDifference 5 sets eight nine
  let five = findSubsetOfDifference 5 sets eight two
  let three = findExcludingFromSet 5 sets two five
  let digits = [zero, one, two, three, four, five, six, seven, eight, nine]

  let outputSet = map Set.fromList $ snd display
  let output = map (fromMaybe 0 . (`elemIndex` digits)) outputSet
  return (positionalNotation output)

part2 :: (Foldable t, Monad t) => t Display -> ConvertedSignal
part2 displays = foldl' (+) 0 $ convertDisplay displays

main :: IO ()
main = do
  displays <- readInput
  putStr "Part one: "
  print (part1 displays)
  putStr "Part two: "
  print (part2 displays)
