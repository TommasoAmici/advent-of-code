import Data.Foldable (Foldable (foldl'))
import Data.List (sort)

type Line = String

readInput :: IO [Line]
readInput = do
  input <- readFile "input.txt"
  return (lines input)

isOpening :: Char -> Bool
isOpening c
  | c == '(' = True
  | c == '[' = True
  | c == '{' = True
  | c == '<' = True
  | otherwise = False

getClosing :: Char -> Char
getClosing c
  | c == '(' = ')'
  | c == '[' = ']'
  | c == '{' = '}'
  | c == '<' = '>'
  | otherwise = '-'

-- We check character by character and add opening characters to a stack.
-- If a closing character is found and it matches the previous open character,
-- we close the pair, remove it from the stack and continue.
findIllegalChar :: Line -> Line -> Char
findIllegalChar [] _ = '-'
findIllegalChar (l : line) [] = if isOpening l then findIllegalChar line [l] else l
findIllegalChar (l : line) (o : opening)
  | isOpening l = findIllegalChar line (l : (o : opening))
  | l == getClosing o = findIllegalChar line opening
  | otherwise = l

syntaxErorrScore :: Char -> Int
syntaxErorrScore c
  | c == ')' = 3
  | c == ']' = 57
  | c == '}' = 1197
  | c == '>' = 25137
  | otherwise = 0

findSyntaxErrorScore :: Line -> Int
findSyntaxErrorScore line = syntaxErorrScore illegalChar
  where
    illegalChar = findIllegalChar line []

filterCorrupt :: [Line] -> [Line]
filterCorrupt = filter (\l -> findIllegalChar l [] == '-')

findCompletion :: Line -> Line -> Line
findCompletion [] opening = map getClosing opening
findCompletion (l : line) []
  | isOpening l = findCompletion line [l]
  | otherwise = [] -- corrupt
findCompletion (l : line) (o : opening)
  | isOpening l = findCompletion line (l : (o : opening))
  | l == getClosing o = findCompletion line opening
  | otherwise = [] -- corrupt

completionScore :: Char -> Int
completionScore c
  | c == ')' = 1
  | c == ']' = 2
  | c == '}' = 3
  | c == '>' = 4
  | otherwise = 0

findCompletionScore :: Line -> Int
findCompletionScore = foldl' (\total c -> total * 5 + completionScore c) 0

findMiddle :: [Int] -> Int
findMiddle xs = xs !! ((length xs -1) `div` 2)

main :: IO ()
main = do
  allLines <- readInput
  putStr "Part one: "
  print (sum $ map findSyntaxErrorScore allLines)

  putStr "Part two: "
  let incompletes = filterCorrupt allLines
  let completionScores = map (findCompletionScore . (`findCompletion` [])) incompletes
  print (findMiddle $ sort completionScores)

-- isCorrupted "[({([[{{<<>>" []
