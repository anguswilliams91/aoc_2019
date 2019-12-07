import System.IO
import Data.List
import Data.List.Split
import qualified Data.Set as Set

step :: [(Int, (Int, Int))] -> String -> [(Int, (Int, Int))]
step xs p
    | d == 'R' = [(j + i, (x + i, y)) | i <- [1..s]] 
    | d == 'L' = [(j + i, (x - i, y)) | i <- [1..s]]
    | d == 'U' = [(j + i, (x, y + i)) | i <- [1..s]]
    | d == 'D' = [(j + i, (x, y - i)) | i <- [1..s]]
    where d = head p
          s = read (tail p) :: Int
          (x, y) = snd (last xs)
          j = fst (last xs)

path :: [String] -> Set.Set (Int, (Int, Int))
path xs = Set.fromList (tail (concat (scanl step [(0, (0, 0))] xs)))

manhattanDistance :: (Int, Int)  -> Int
manhattanDistance (a, b) = abs a + abs b

closestIntersection :: Set.Set (Int, (Int, Int)) -> Set.Set (Int, (Int, Int)) -> Int
closestIntersection xs ys = minimum (Set.map manhattanDistance (Set.intersection xs1 ys1))
    where xs1 = Set.map snd xs
          ys1 = Set.map snd ys

closestBySteps :: Set.Set (Int, (Int, Int)) -> Set.Set (Int, (Int, Int)) -> Int
closestBySteps xs ys = head [fst x + fst y | x <- Set.toList xss, y <- Set.toList yss, snd x == snd y]
    where zs = Set.intersection xs1 ys1
          xs1 = Set.map snd xs
          ys1 = Set.map snd ys
          f x = Set.member (snd x) zs
          xss = Set.filter f xs
          yss = Set.filter f ys   

main = do
    contents <- readFile "../inputs/dayThree.txt"
    let directions = map (splitOn ",") (lines contents)
    let p1 = path (head directions)
    let p2 = path (last directions)
    let partOne = closestIntersection p1 p2
    let partTwo = closestBySteps p1 p2
    print partOne
    print partTwo



