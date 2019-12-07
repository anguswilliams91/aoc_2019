import System.IO
import Data.List
import Data.List.Split
import qualified Data.Set as Set

step :: [(Int, Int)] -> String -> [(Int, Int)]
step xs p
    | d == 'R' = [(x + i, y) | i <- [1..s]] 
    | d == 'L' = [(x - i, y) | i <- [1..s]]
    | d == 'U' = [(x, y + i) | i <- [1..s]]
    | d == 'D' = [(x, y - i) | i <- [1..s]]
    where d = head p
          s = read (tail p) :: Int
          (x, y) = last xs

path :: [String] -> Set.Set (Int, Int)
path xs = Set.fromList (tail (concat (scanl step [(0, 0)] xs)))

manhattanDistance :: (Int, Int)  -> Int
manhattanDistance (a, b) = abs a + abs b

closestIntersection :: Set.Set (Int, Int) -> Set.Set (Int, Int) -> Int
closestIntersection xs ys = minimum (Set.map manhattanDistance (Set.intersection xs ys))

main = do
    contents <- readFile "../inputs/dayThree.txt"
    let directions = map (splitOn ",") (lines contents)
    let p1 = path (head directions)
    let p2 = path (last directions)
    let partOne = closestIntersection p1 p2
    print partOne



