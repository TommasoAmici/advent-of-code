import Data.Char (isLower)
import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Vertex = [Char]

type Graph = M.Map Vertex [Vertex]

type Path = [Vertex]

readInput :: IO Graph
readInput = do
  input <- readFile "input.txt"
  return (parseInput input)

parseInput :: String -> Graph
parseInput input = edges
  where
    inputLines = lines input
    edges = parseEdges inputLines M.empty

parseEdges :: [Vertex] -> Graph -> Graph
parseEdges [] m = m
parseEdges (x : xs) m = parseEdges xs insertEnd
  where
    split = splitOn "-" x
    start = head split
    end = head (tail split)
    insertStart = M.insertWith (++) start [end] m
    insertEnd = M.insertWith (++) end [start] insertStart

isSmallCave :: Vertex -> Bool
isSmallCave = all ((== True) . isLower)

isAlreadyVisited :: Vertex -> Path -> Bool
isAlreadyVisited = elem

isValidPath :: Path -> Bool
isValidPath [] = False
isValidPath path = start == "start" && end == "end"
  where
    start = head path
    end = last path

reachable :: Graph -> Vertex -> [Vertex]
reachable graph "end" = []
reachable graph vertex = fromMaybe [] (M.lookup vertex graph)

findNextPart1 :: Graph -> Path -> [Path]
findNextPart1 _ [] = []
findNextPart1 graph path =
  [ path <> [next]
    | next <- reachable graph (last path),
      not (isSmallCave next && isAlreadyVisited next path)
  ]

visitedSmallTwice :: Path -> Vertex -> Bool
visitedSmallTwice [] _ = False
visitedSmallTwice path "start" = True -- start is already visited by definition
visitedSmallTwice _ "end" = False -- finished paths will be filtered out after this
visitedSmallTwice path next = length smallCaves > length (nub smallCaves) + 1
  where
    smallCaves = filter isSmallCave (path <> [next])

findNextPart2 :: Graph -> Path -> [Path]
findNextPart2 _ [] = []
findNextPart2 graph path =
  [ path <> [next]
    | next <- reachable graph (last path),
      not (isSmallCave next && visitedSmallTwice path next)
  ]

findAllPaths :: (Graph -> Path -> [Path]) -> Graph -> [Path] -> [Path] -> [Path]
findAllPaths findNext _ completed [] = completed
findAllPaths findNext graph completed paths = findAllPaths findNext graph (valid <> completed) pending
  where
    invalid = filter (not . isValidPath) paths
    pending = concatMap (findNext graph) invalid
    valid = filter isValidPath pending

main :: IO ()
main = do
  graph <- readInput
  putStr "Part one: "
  print (length (findAllPaths findNextPart1 graph [] [["start"]]))
  putStr "Part two: "
  print (length (findAllPaths findNextPart2 graph [] [["start"]]))
