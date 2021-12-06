{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl', maximumBy, minimumBy, transpose)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Extracted = Int

type ExtractionRound = Int

type Cell = (Extracted, ExtractionRound)

type Board = [[Cell]]

readValues :: IO (Map.Map Extracted ExtractionRound, Map.Map ExtractionRound Extracted, [Board])
readValues = do
  input <- TIO.readFile "input.txt"
  let lines = T.splitOn "\n\n" input
  let extracted = parseExtracted (head lines)
  let extractedToRound = extractedToRoundMap extracted
  let roundToExtracted = roundToExtractedMap extracted
  let boards = parseBoards (tail lines)
  return (extractedToRound, roundToExtracted, boards)

parseExtracted :: T.Text -> [Extracted]
parseExtracted extracted = map (read . T.unpack) (T.splitOn "," extracted)

extractedToRoundMap :: [Extracted] -> Map.Map Extracted ExtractionRound
extractedToRoundMap extracted = Map.fromList (zip extracted [0 ..])

roundToExtractedMap :: [Extracted] -> Map.Map ExtractionRound Extracted
roundToExtractedMap extracted = Map.fromList (zip [0 ..] extracted)

parseCell :: T.Text -> Cell
parseCell x = (read (T.unpack x), -1)

parseBoards :: Monad m => m T.Text -> m [[Cell]]
parseBoards raws = do
  raw <- raws
  let rows = T.lines raw
  let cells = map T.words rows
  return (map (map parseCell) cells)

justOrMinusOne :: Num p => Maybe p -> p
justOrMinusOne (Just val) = val
justOrMinusOne Nothing = -1

fillExtractionRound :: Map.Map Extracted ExtractionRound -> Board -> Board
fillExtractionRound extractionRounds = map (map (\(num, _) -> (num, justOrMinusOne (Map.lookup num extractionRounds))))

findWinningRoundPerRow :: Board -> ExtractionRound
findWinningRoundPerRow filledBoard = snd firstCompletedRow
  where
    completedRows = map (maximumBy (comparing snd)) filledBoard
    firstCompletedRow = minimumBy (comparing snd) completedRows

findWinningRoundPerColumn :: Board -> ExtractionRound
findWinningRoundPerColumn filledBoard = findWinningRoundPerRow (transpose filledBoard)

findWinningBoard :: Ord a => (((a, b) -> (a, b) -> Ordering) -> [(ExtractionRound, Board)] -> (ExtractionRound, Board)) -> Map.Map Extracted ExtractionRound -> [Board] -> (ExtractionRound, Board)
findWinningBoard compFun extractionRounds boards = winningBoard
  where
    withWinningRound =
      map
        ( \board ->
            let filled = fillExtractionRound extractionRounds board
             in (minimum [findWinningRoundPerRow filled, findWinningRoundPerColumn filled], filled)
        )
        boards
    winningBoard = compFun (comparing fst) withWinningRound

findEarliestWinningBoard :: Map.Map Extracted ExtractionRound -> [Board] -> (ExtractionRound, Board)
findEarliestWinningBoard = findWinningBoard minimumBy

findLastWinningBoard :: Map.Map Extracted ExtractionRound -> [Board] -> (ExtractionRound, Board)
findLastWinningBoard = findWinningBoard maximumBy

countBoardScore :: Maybe Extracted -> ExtractionRound -> Board -> Int
countBoardScore Nothing _ _ = -1
countBoardScore (Just finalExtracted) finalRound board = final
  where
    final = sumUnmarkedCells * finalExtracted
    unmarkedCells = filter (\(_, r) -> r > finalRound) (concat board)
    sumUnmarkedCells = foldl' (+) 0 (map fst unmarkedCells)

main :: IO ()
main = do
  (extractedToRound, roundToExtracted, boards) <- readValues
  let (winningRound, winningBoard) = findEarliestWinningBoard extractedToRound boards
  putStr "Part one: "
  print (countBoardScore (Map.lookup winningRound roundToExtracted) winningRound winningBoard)
  putStr "Part two: "
  let (lastWinningRound, lastWinningBoard) = findLastWinningBoard extractedToRound boards
  print (countBoardScore (Map.lookup lastWinningRound roundToExtracted) lastWinningRound lastWinningBoard)
